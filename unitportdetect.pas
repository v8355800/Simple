{
  PortDetect V1.0 (17th June 2007)
  Delphi detection unit for LPT and COM port names and resources
  by Tommy Sools - public domain

  On all operating systems except Windows 95, the friendly names and
  I/O addresses of all COM and LPT ports will be detected.
  Only the first I/O address range will be returned.
  In most cases the unit will also succeed in detecting IRQ numbers and
  DMA addresses.

  On the Windows 95 operating system, for COM ports only the numbers of the
  available ports will be detected and for LPT ports only the I/O addresses
  of LPT1 up to LPT3 will be detected.

  Uses WDM based device detection (using SetupDi API) for:
  - Windows XP
    (tested with 1 x PCI LPT (normal) & 2 x PCI LPT (Netmos/Moschip) & 1 x USB COM & 4 x virtual COM)
  - Windows 2000
  - Windows Millennium Edition
  - Windows 98 Second Edition
    (tested with 3 x ISA LPT & 2 x ISA COM & 1 x USB COM & 2 x virtual COM)
  - Windows 98 (untested, if not WDM compatible then it will use the Windows 95 approach)

  Uses registry based device detection for:
  - Windows 2000 legacy ports (ISA)
  - Windows NT (tested with 3 x ISA LPT & 2 x PCI LPT (Netmos/Moschip) & 2 x ISA COM & 4 x virtual COM)

  Uses reduced device detection (using EnumPorts API and Toolhelp32ReadProcessMemory) for:
  - Windows 95

  The detection methods are not strictly bound to specific operating systems.
  All detection methods will be attempted in the above order, until a detection
  method succeeds. For example, if WDM based detection fails on Windows XP, it
  will fall back to registry based detection, which will also work in most cases.
}

unit unitportdetect;

interface

uses
  Windows;

type
  TDetectedPort = record
    friendlyName: string;
    portStart: Int64;
    portLength: LongWord;
    irq: Boolean;
    irqLevel: LongWord;
    dma: Boolean;
    dmaChannel: LongWord;
  end;

function getCOMPorts(var ports: array of TDetectedPort): Boolean;
function getLPTPorts(var ports: array of TDetectedPort): Boolean;
function getPortsWDM(var ports: array of TDetectedPort; guid1: TGUID; portPrefix: string): Boolean;
function getPortsNT(var ports: array of TDetectedPort; service: string; portPrefix: string; var legacyFound: Boolean): Boolean;
function getPorts95(var ports: array of TDetectedPort; portPrefix: string): Boolean;
function getLPTPorts95(var ports: array of TDetectedPort): Boolean;

implementation

uses
  SysUtils, Classes, Registry, Winspool;

const
  kernel32 = 'kernel32.dll';

var
  toolhelp32ReadProcessMemory: function(th32ProcessID: DWORD; lpBaseAddress: Pointer; var lpBuffer; cbRead: DWORD; var lpNumberOfBytesRead: DWORD): LongBool; stdcall;
  hKernel32: HINST;

const
  GUID_CLASS_COMPORT:          TGUID = '{86E0D1E0-8089-11D0-9CE4-08003E301F73}';
  GUID_SERENUM_BUS_ENUMERATOR: TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';
  GUID_PARALLEL_DEVICE:        TGUID = '{97F76EF0-F883-11D0-AF1F-0000F800845C}';
  // these are not really needed but might be helpful for future expansion
  GUID_PARCLASS_DEVICE:        TGUID = '{811FC6A5-F728-11D0-A537-0000F8753ED1}';
  GUID_CLASS_MOUSE:            TGUID = '{378de44c-56ef-11d1-bc8c-00a0c91405dd}';
  GUID_CLASS_INPUT:            TGUID = '{4D1E55B2-F16F-11CF-88CB-001111000030}';
  GUID_CLASS_KEYBOARD:         TGUID = '{884b96c3-56ef-11d1-bc8c-00a0c91405dd}';
  GUID_CLASS_USBHUB:           TGUID = '{f18a0e88-c30c-11d0-8815-00a0c906bed8}';
  GUID_CLASS_USB_DEVICE:       TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_DEVICE_BATTERY:         TGUID = '{72631e54-78A4-11d0-bcf7-00aa00b7b32a}';
  GUID_DEVICE_SYS_BUTTON:      TGUID = '{4AFA3D53-74A7-11d0-be5e-00A0C9062857}';
  GUID_DEVICE_LID:             TGUID = '{4AFA3D52-74A7-11d0-be5e-00A0C9062857}';
  GUID_DEVICE_THERMAL_ZONE:    TGUID = '{4AFA3D51-74A7-11d0-be5e-00A0C9062857}';
  GUID_DEVCLASS_1394:          TGUID = '{6bdd1fc1-810f-11d0-bec7-08002be2092f}';
  GUID_DEVCLASS_ADAPTER:       TGUID = '{4d36e964-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_APMSUPPORT:    TGUID = '{d45b1c18-c8fa-11d1-9f77-0000f805f530}';
  GUID_DEVCLASS_BATTERY:       TGUID = '{72631e54-78a4-11d0-bcf7-00aa00b7b32a}';
  GUID_DEVCLASS_CDROM:         TGUID = '{4d36e965-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_COMPUTER:      TGUID = '{4d36e966-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_DECODER:       TGUID = '{6bdd1fc2-810f-11d0-bec7-08002be2092f}';
  GUID_DEVCLASS_DISKDRIVE:     TGUID = '{4d36e967-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_DISPLAY:       TGUID = '{4d36e968-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_FDC:           TGUID = '{4d36e969-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_FLOPPYDISK:    TGUID = '{4d36e980-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_GPS:           TGUID = '{6bdd1fc3-810f-11d0-bec7-08002be2092f}';
  GUID_DEVCLASS_HDC:           TGUID = '{4d36e96a-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_HIDCLASS:      TGUID = '{745a17a0-74d3-11d0-b6fe-00a0c90f57da}';
  GUID_DEVCLASS_IMAGE:         TGUID = '{6bdd1fc6-810f-11d0-bec7-08002be2092f}';
  GUID_DEVCLASS_INFRARED:      TGUID = '{6bdd1fc5-810f-11d0-bec7-08002be2092f}';
  GUID_DEVCLASS_KEYBOARD:      TGUID = '{4d36e96b-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_LEGACYDRIVER:  TGUID = '{8ecc055d-047f-11d1-a537-0000f8753ed1}';
  GUID_DEVCLASS_MEDIA:         TGUID = '{4d36e96c-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MEDIUM_CHANGER: TGUID = '{ce5939ae-ebde-11d0-b181-0000f8753ec4}';
  GUID_DEVCLASS_MODEM:         TGUID = '{4d36e96d-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MONITOR:       TGUID = '{4d36e96e-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MOUSE:         TGUID = '{4d36e96f-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MTD:           TGUID = '{4d36e970-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MULTIFUNCTION: TGUID = '{4d36e971-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_MULTIPORTSERIAL: TGUID = '{50906cb8-ba12-11d1-bf5d-0000f805f530}';
  GUID_DEVCLASS_NET:           TGUID = '{4d36e972-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_NETCLIENT:     TGUID = '{4d36e973-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_NETSERVICE:    TGUID = '{4d36e974-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_NETTRANS:      TGUID = '{4d36e975-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_NODRIVER:      TGUID = '{4d36e976-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_PCMCIA:        TGUID = '{4d36e977-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_PORTS:         TGUID = '{4d36e978-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_PRINTER:       TGUID = '{4d36e979-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_PRINTERUPGRADE: TGUID = '{4d36e97a-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_SCSIADAPTER:   TGUID = '{4d36e97b-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_SMARTCARDREADER: TGUID = '{50dd5230-ba8a-11d1-bf5d-0000f805f530}';
  GUID_DEVCLASS_SOUND:         TGUID = '{4d36e97c-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_SYSTEM:        TGUID = '{4d36e97d-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_TAPEDRIVE:     TGUID = '{6d807884-7d21-11cf-801c-08002be10318}';
  GUID_DEVCLASS_UNKNOWN:       TGUID = '{4d36e97e-e325-11ce-bfc1-08002be10318}';
  GUID_DEVCLASS_USB:           TGUID = '{36fc9e60-c465-11cf-8056-444553540000}';
  GUID_DEVCLASS_VOLUME:        TGUID = '{71a27cdd-812a-11d0-bec7-08002be2092f}';

function getCOMPorts(var ports: array of TDetectedPort): Boolean;
var
  i: Integer;
  resultWDM, resultNT, legacyFound, result95: Boolean;
begin
  for i := 0 to Length(ports) - 1 do
    ZeroMemory(@ports[i], sizeof(ports[i]));
  // detect plug & play COM ports (normal, USB, virtual)
  resultWDM := getPortsWDM(ports, GUID_DEVCLASS_PORTS, 'COM');
  // compatibility with Windows NT and Windows 2000 legacy ports by probing registry
  resultNT := getPortsNT(ports, 'Serial', 'COM', legacyFound); // Microsoft serial port arbitrator
  // on Windows 95/98/Millennium platforms, detect friendly names for all ports
  // (this way virtual COM ports will also be detected)
  if (GetVersion() >= $80000000) or not(resultWDM or resultNT) then
    result95 := getPorts95(ports, 'COM')
  else
    result95 := False;
  Result := resultWDM or resultNT or result95;
end;

function getLPTPorts(var ports: array of TDetectedPort): Boolean;
var
  i: Integer;
  legacyFound: Boolean;
begin
  for i := 0 to Length(ports) - 1 do
    ZeroMemory(@ports[i], sizeof(ports[i]));
  // detect plug & play LPT ports (normal and Netmos/Moschip)
  Result := getPortsWDM(ports, GUID_DEVCLASS_PORTS, 'LPT');
  // compatibility with Windows NT and Windows 2000 legacy ports by probing registry
  Result := getPortsNT(ports, 'Parport', 'LPT', legacyFound) or Result; // Microsoft parallel port arbitrator
  Result := getPortsNT(ports, 'NmPar', 'LPT', legacyFound) or Result; // NetMos/Moschip parallel port arbitrator
  // compatibility with Windows 95 by probing memory
  if not(Result) then
  begin
    // detect friendly names for all LPT ports
    Result := getPorts95(ports, 'LPT');
    // on Windows 9X platforms, detect I/O addresses for LPT1 up to LPT3
    if GetVersion() >= $80000000 then
      Result := getLPTPorts95(ports);
  end;
end;

{$ALIGN 1}

const
  setupapi = 'setupapi.dll';

const
  DIGCF_DEFAULT         = $00000001;  // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT         = $00000002;
  DIGCF_ALLCLASSES      = $00000004;
  DIGCF_PROFILE         = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

type
  GUID = TGUID;
  ULONG_PTR = PULONG;
  HDEVINFO = THandle;
  PVOID = Pointer;

  SP_DEVINFO_DATA = record
      cbSize: DWORD;
      ClassGuid: GUID;
      DevInst: DWORD;    // DEVINST handle
      Reserved: ULONG_PTR;
  end;
  PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;

  SP_DEVICE_INTERFACE_DATA = record
    cbSize: DWORD;
    InterfaceClassGuid: GUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  PSP_DEVICE_INTERFACE_DATA = ^SP_DEVICE_INTERFACE_DATA;

const
  SPINT_ACTIVE  = $00000001;
  SPINT_DEFAULT = $00000002;
  SPINT_REMOVED = $00000004;

type
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = record
    cbSize: DWORD;
    //DevicePath: array of CHAR;
  end;
  PSP_DEVICE_INTERFACE_DETAIL_DATA_A = ^SP_DEVICE_INTERFACE_DETAIL_DATA_A;

const
  SPDRP_DEVICEDESC                  = $00000000;
  SPDRP_HARDWAREID                  = $00000001;
  SPDRP_COMPATIBLEIDS               = $00000002;
  SPDRP_UNUSED0                     = $00000003;
  SPDRP_SERVICE                     = $00000004;
  SPDRP_UNUSED1                     = $00000005;
  SPDRP_UNUSED2                     = $00000006;
  SPDRP_CLASS                       = $00000007;
  SPDRP_CLASSGUID                   = $00000008;
  SPDRP_DRIVER                      = $00000009;
  SPDRP_CONFIGFLAGS                 = $0000000A;
  SPDRP_MFG                         = $0000000B;
  SPDRP_FRIENDLYNAME                = $0000000C;
  SPDRP_LOCATION_INFORMATION        = $0000000D;
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E;
  SPDRP_CAPABILITIES                = $0000000F;
  SPDRP_UI_NUMBER                   = $00000010;
  SPDRP_UPPERFILTERS                = $00000011;
  SPDRP_LOWERFILTERS                = $00000012;
  SPDRP_BUSTYPEGUID                 = $00000013;
  SPDRP_LEGACYBUSTYPE               = $00000014;
  SPDRP_BUSNUMBER                   = $00000015;
  SPDRP_ENUMERATOR_NAME             = $00000016;
  SPDRP_SECURITY                    = $00000017;
  SPDRP_SECURITY_SDS                = $00000018;
  SPDRP_DEVTYPE                     = $00000019;
  SPDRP_EXCLUSIVE                   = $0000001A;
  SPDRP_CHARACTERISTICS             = $0000001B;
  SPDRP_ADDRESS                     = $0000001C;
  SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001D;
  SPDRP_DEVICE_POWER_DATA           = $0000001E;
  SPDRP_REMOVAL_POLICY              = $0000001F;
  SPDRP_REMOVAL_POLICY_HW_DEFAULT   = $00000020;
  SPDRP_REMOVAL_POLICY_OVERRIDE     = $00000021;
  SPDRP_INSTALL_STATE               = $00000022;
  SPDRP_LOCATION_PATHS              = $00000023;
  SPDRP_MAXIMUM_PROPERTY            = $00000024;

  DICS_ENABLE     = $00000001;
  DICS_DISABLE    = $00000002;
  DICS_PROPCHANGE = $00000003;
  DICS_START      = $00000004;
  DICS_STOP       = $00000005;

  DICS_FLAG_GLOBAL         = $00000001;
  DICS_FLAG_CONFIGSPECIFIC = $00000002;
  DICS_FLAG_CONFIGGENERAL  = $00000004;

  DIREG_DEV  = $00000001;
  DIREG_DRV  = $00000002;
  DIREG_BOTH = $00000004;

  DIOCR_INSTALLER = $00000001;
  DIOCR_INTERFACE = $00000002;

var
  hSetupapi: HINST;
  SetupDiClassGuidsFromNameA: function(
    ClassName: LPCTSTR;
    ClassGuidList: PGUID;
    ClassGuidListSize: DWORD;
    RequiredSize: PDWORD
  ): LongBool; stdcall;
  SetupDiGetClassDevsA: function(
    ClassGuid: PGUID;
    Enumerator: LPCSTR;
    hwndParent: HWND;
    Flags: DWORD
  ): HDEVINFO; stdcall;
  SetupDiDestroyDeviceInfoList: function(
    DeviceInfoSet: HDEVINFO
  ): LongBool; stdcall;
  SetupDiEnumDeviceInfo: function(
    DeviceInfoSet: HDEVINFO;
    MemberIndex: DWORD;
    DeviceInfoData: PSP_DEVINFO_DATA
  ): LongBool; stdcall;
  SetupDiEnumDeviceInterfaces: function(
    DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA;
    InterfaceClassGuid: PGUID;
    MemberIndex: DWORD;
    DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA
  ): LongBool; stdcall;
  SetupDiGetDeviceInterfaceDetailA: function(
    DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA;
    DeviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA_A;
    DeviceInterfaceDetailDataSize: DWORD;
    RequiredSize: PDWORD;
    DeviceInfoData: PSP_DEVINFO_DATA
  ): LongBool; stdcall;
  SetupDiGetDeviceInstanceIdA: function(
    DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA;
    DeviceInstanceId: LPSTR;
    DeviceInstanceIdSize: DWORD;
    RequiredSize: PDWORD
  ): LongBool; stdcall;
  SetupDiGetDeviceRegistryPropertyA: function(
    DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA;
    Property1: DWORD;
    PropertyRegDataType: PDWORD;
    PropertyBuffer: PBYTE;
    PropertyBufferSize: DWORD;
    RequiredSize: PDWORD
  ): LongBool; stdcall;
  SetupDiOpenClassRegKeyExA: function(
    ClassGuid: PGUID;
    samDesired: REGSAM;
    Flags: DWORD;
    MachineName: LPCTSTR;
    Reserved: PVOID
  ): HKEY; stdcall;
  SetupDiOpenDevRegKey: function(
    DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA;
    Scope: DWORD;
    HwProfile: DWORD;
    KeyType: DWORD;
    samDesired: REGSAM
  ): HKEY; stdcall;

{$ALIGN 8}

{$ALIGN 1}

const
  cfgmgr32 = 'cfgmgr32.dll';

const
  BASIC_LOG_CONF    = $00000000;
  FILTERED_LOG_CONF = $00000001;
  ALLOC_LOG_CONF    = $00000002;
  BOOT_LOG_CONF     = $00000003;
  FORCED_LOG_CONF   = $00000004;
  OVERRIDE_LOG_CONF = $00000005;
  NUM_LOG_CONF      = $00000006;
  LOG_CONF_BITS     = $00000007;

  CR_SUCCESS                  = $00000000;
  CR_DEFAULT                  = $00000001;
  CR_OUT_OF_MEMORY            = $00000002;
  CR_INVALID_POINTER          = $00000003;
  CR_INVALID_FLAG             = $00000004;
  CR_INVALID_DEVNODE          = $00000005;
  CR_INVALID_DEVINST          = CR_INVALID_DEVNODE;
  CR_INVALID_RES_DES          = $00000006;
  CR_INVALID_LOG_CONF         = $00000007;
  CR_INVALID_ARBITRATOR       = $00000008;
  CR_INVALID_NODELIST         = $00000009;
  CR_DEVNODE_HAS_REQS         = $0000000A;
  CR_DEVINST_HAS_REQS         = CR_DEVNODE_HAS_REQS;
  CR_INVALID_RESOURCEID       = $0000000B;
  CR_DLVXD_NOT_FOUND          = $0000000C;
  CR_NO_SUCH_DEVNODE          = $0000000D;
  CR_NO_SUCH_DEVINST          = CR_NO_SUCH_DEVNODE;
  CR_NO_MORE_LOG_CONF         = $0000000E;
  CR_NO_MORE_RES_DES          = $0000000F;
  CR_ALREADY_SUCH_DEVNODE     = $00000010;
  CR_ALREADY_SUCH_DEVINST     = CR_ALREADY_SUCH_DEVNODE;
  CR_INVALID_RANGE_LIST       = $00000011;
  CR_INVALID_RANGE            = $00000012;
  CR_FAILURE                  = $00000013;
  CR_NO_SUCH_LOGICAL_DEV      = $00000014;
  CR_CREATE_BLOCKED           = $00000015;
  CR_NOT_SYSTEM_VM            = $00000016;
  CR_REMOVE_VETOED            = $00000017;
  CR_APM_VETOED               = $00000018;
  CR_INVALID_LOAD_TYPE        = $00000019;
  CR_BUFFER_SMALL             = $0000001A;
  CR_NO_ARBITRATOR            = $0000001B;
  CR_NO_REGISTRY_HANDLE       = $0000001C;
  CR_REGISTRY_ERROR           = $0000001D;
  CR_INVALID_DEVICE_ID        = $0000001E;
  CR_INVALID_DATA             = $0000001F;
  CR_INVALID_API              = $00000020;
  CR_DEVLOADER_NOT_READY      = $00000021;
  CR_NEED_RESTART             = $00000022;
  CR_NO_MORE_HW_PROFILES      = $00000023;
  CR_DEVICE_NOT_THERE         = $00000024;
  CR_NO_SUCH_VALUE            = $00000025;
  CR_WRONG_TYPE               = $00000026;
  CR_INVALID_PRIORITY         = $00000027;
  CR_NOT_DISABLEABLE          = $00000028;
  CR_FREE_RESOURCES           = $00000029;
  CR_QUERY_VETOED             = $0000002A;
  CR_CANT_SHARE_IRQ           = $0000002B;
  CR_NO_DEPENDENT             = $0000002C;
  CR_SAME_RESOURCES           = $0000002D;
  CR_NO_SUCH_REGISTRY_KEY     = $0000002E;
  CR_INVALID_MACHINENAME      = $0000002F;
  CR_REMOTE_COMM_FAILURE      = $00000030;
  CR_MACHINE_UNAVAILABLE      = $00000031;
  CR_NO_CM_SERVICES           = $00000032;
  CR_ACCESS_DENIED            = $00000033;
  CR_CALL_NOT_IMPLEMENTED     = $00000034;
  CR_INVALID_PROPERTY         = $00000035;
  CR_DEVICE_INTERFACE_ACTIVE  = $00000036;
  CR_NO_SUCH_DEVICE_INTERFACE = $00000037;
  CR_INVALID_REFERENCE_STRING = $00000038;
  CR_INVALID_CONFLICT_LIST    = $00000039;
  CR_INVALID_INDEX            = $0000003A;
  CR_INVALID_STRUCTURE_SIZE   = $0000003B;
  NUM_CR_RESULTS              = $0000003C;

  ResType_All           = $00000000;
  ResType_None          = $00000000;
  ResType_Mem           = $00000001;
  ResType_IO            = $00000002;
  ResType_DMA           = $00000003;
  ResType_IRQ           = $00000004;
  ResType_DoNotUse      = $00000005;
  ResType_BusNumber     = $00000006;
  ResType_MAX           = $00000006;
  ResType_Ignored_Bit   = $00008000;
  ResType_ClassSpecific = $0000FFFF;
  ResType_Reserved      = $00008000;
  ResType_DevicePrivate = $00008001;
  ResType_PcCardConfig  = $00008002;
  ResType_MfCardConfig  = $00008003;

type
  DWORD_PTR = DWORD;
  LOG_CONF = DWORD_PTR;
  PLOG_CONF = ^LOG_CONF;
  DEVNODE = DWORD;
  PDEVNOD = ^DEVNODE;
  DEVINST = DWORD;
  PDEVINST = ^DEVINST;
  RETURN_TYPE = DWORD;
  CONFIGRET = RETURN_TYPE;
  RES_DES = DWORD_PTR;
  PRES_DES = ^RES_DES;
  RESOURCEID = ULONG;
  PRESOURCEID = ^RESOURCEID;
  DWORDLONG = Int64;
  ULONG32 = ULONG;
  IO_DES = record
    IOD_Count: DWORD;
    IOD_Type: DWORD;
    IOD_Alloc_Base: DWORDLONG;
    IOD_Alloc_End: DWORDLONG;
    IOD_DesFlags: DWORD;
  end;
  PIO_DES = ^IO_DES;
  IO_RANGE = record
   IOR_Align: DWORDLONG;
   IOR_nPorts: DWORD;
   IOR_Min: DWORDLONG;
   IOR_Max: DWORDLONG;
   IOR_RangeFlags: DWORD;
   IOR_Alias: DWORDLONG;
  end;
  PIO_RANGE = ^IO_RANGE;
  IO_RESOURCE = record
    IO_Header: IO_DES;
    IO_Data: array of IO_RANGE;
  end;
  PIO_RESOURCE = ^IO_RESOURCE;
  DMA_RANGE = record
    DR_Min: ULONG;
    DR_Max: ULONG;
    DR_Flags: ULONG;
  end;
  PDMA_RANGE = ^DMA_RANGE;
  DMA_DES = record
   DD_Count: DWORD;
   DD_Type: DWORD;
   DD_Flags: DWORD;
   DD_Alloc_Chan: ULONG;
  end;
  PDMA_DES = ^DMA_DES;
  DMA_RESOURCE = record
   DMA_Header: DMA_DES;
   DMA_Data: array of DMA_RANGE;
  end;
  PDMA_RESOURCE = ^DMA_RESOURCE;
  IRQ_RANGE = record
    IRQR_Min: ULONG;
    IRQR_Max: ULONG;
    IRQR_Flags: ULONG;
  end;
  PIRQ_RANGE = ^IRQ_RANGE;
  IRQ_DES_32 = record
   IRQD_Count: DWORD;    
   IRQD_Type: DWORD;
   IRQD_Flags: DWORD;
   IRQD_Alloc_Num: ULONG;
   IRQD_Affinity: ULONG32;
  end;
  PIRQ_DES_32 = ^IRQ_DES_32;
  IRQ_RESOURCE_32 = record
    IRQ_Header: IRQ_DES_32;
    IRQ_Data: array of IRQ_RANGE;
  end;
  PIRQ_RESOURCE_32 = ^IRQ_RESOURCE_32;

var
  hCfgmgr32: HINST;
  CM_Get_First_Log_Conf: function(
    plcLogConf: PLOG_CONF;
    dnDevInst: DEVINST;
    ulFlags: ULONG
  ): CONFIGRET; stdcall;
  CM_Free_Log_Conf_Handle: function(
    lcLogConf: LOG_CONF
    ): CONFIGRET; stdcall;
  CM_Get_Next_Res_Des: function(
    prdResDes: PRES_DES;
    rdResDes: RES_DES;
    ForResource: RESOURCEID;
    pResourceID: PRESOURCEID;
    ulFlags: ULONG
  ): CONFIGRET; stdcall;
  CM_Free_Res_Des_Handle: function(
    rdResDes: RES_DES
  ): CONFIGRET; stdcall;
  CM_Get_Res_Des_Data_Size: function(
    pulSize: PULONG;
    rdResDes: RES_DES;
    ulFlags: ULONG
  ): CONFIGRET; stdcall;
  CM_Get_Res_Des_Data: function(
    rdResDes: RES_DES;
    Buffer: Pointer;
    BufferLen: ULONG;
    ulFlags: ULONG
  ): CONFIGRET; stdcall;

{$ALIGN 8}

function extractResourceDesc(var port: TDetectedPort; resId: RESOURCEID; buffer: PChar; length: Integer): Boolean;
var
  ioResource: IO_RESOURCE;
  dmaResource: DMA_RESOURCE;
  irqResource: IRQ_RESOURCE_32;
  i, p, l: Integer;
begin
  Result := False;
  case resId of
    ResType_IO:
    begin
      p := 0;
      l := SizeOf(ioResource) - SizeOf(ioResource.IO_Data);
      if length >= p + l then
      begin
        Move((buffer + p)^, ioResource, l);
        Inc(p, l);
        SetLength(ioResource.IO_Data, ioResource.IO_Header.IOD_Count);
        for i := 0 to ioResource.IO_Header.IOD_Count - 1 do
        begin
          l := SizeOf(ioResource.IO_Data[i]);
          if length >= p + l then
          begin
            Move((buffer + p)^, ioResource.IO_Data[i], l);
            Inc(p, l);
          end;
        end;
        if port.portLength = 0 then
        begin
          port.portStart := ioResource.IO_Header.IOD_Alloc_Base;
          port.portLength := ioResource.IO_Header.IOD_Alloc_End - ioResource.IO_Header.IOD_Alloc_Base + 1;
        end;
        Result := True;
      end;
    end;
    ResType_DMA:
    begin
      p := 0;
      l := SizeOf(dmaResource) - SizeOf(dmaResource.DMA_Data);
      if length >= p + l then
      begin
        Move((buffer + p)^, dmaResource, l);
        Inc(p, l);
        SetLength(dmaResource.DMA_Data, dmaResource.DMA_Header.DD_Count);
        for i := 0 to dmaResource.DMA_Header.DD_Count - 1 do
        begin
          l := SizeOf(dmaResource.DMA_Data[i]);
          if length >= p + l then
          begin
            Move((buffer + p)^, dmaResource.DMA_Data[i], l);
            Inc(p, l);
          end;
        end;
        if not(port.dma) then
        begin
          port.dma := True;
          port.dmaChannel := dmaResource.DMA_Header.DD_Alloc_Chan;
        end;
        Result := True;
      end;
    end;
    ResType_IRQ:
    begin
      p := 0;
      l := SizeOf(irqResource) - SizeOf(irqResource.IRQ_Data);
      if length >= p + l then
      begin
        Move((buffer + p)^, irqResource, l);
        Inc(p, l);
        SetLength(irqResource.IRQ_Data, irqResource.IRQ_Header.IRQD_Count);
        for i := 0 to irqResource.IRQ_Header.IRQD_Count - 1 do
        begin
          l := SizeOf(irqResource.IRQ_Data[i]);
          if length >= p + l then
          begin
            Move((buffer + p)^, irqResource.IRQ_Data[i], l);
            Inc(p, l);
          end;
        end;
        if not(port.irq) then
        begin
          port.irq := True;
          port.irqLevel := irqResource.IRQ_Header.IRQD_Alloc_Num;
        end;
        Result := True;
      end;
    end;
  end;
end;

procedure getPortResourcesWDMConfigManager(var port: TDetectedPort; devInst1: DEVINST);
var
  logConf: LOG_CONF;
  resDesc, resDescPrev: RES_DES;
  resId: RESOURCEID;
  size, resDescBufferSize: ULONG;
  resDescBuffer: PChar;
begin
  if (@CM_Get_First_Log_Conf <> nil)
     and
     (@CM_Free_Log_Conf_Handle <> nil)
     and
     (@CM_Get_Next_Res_Des <> nil)
     and
     (@CM_Free_Res_Des_Handle <> nil)
     and
     (@CM_Get_Res_Des_Data_Size <> nil)
     and
     (@CM_Get_Res_Des_Data <> nil) then
  begin
    if CM_Get_First_Log_Conf(@logConf, devInst1, ALLOC_LOG_CONF) = CR_SUCCESS then
    begin
      resDescPrev := INVALID_HANDLE_VALUE;
      if CM_Get_Next_Res_Des(@resDesc, logConf, ResType_All, @resId, 0) = CR_SUCCESS then
      begin
        resDescBufferSize := 100;
        GetMem(resDescBuffer, resDescBufferSize);
        try
          repeat
            if resDescPrev <> INVALID_HANDLE_VALUE then
              CM_Free_Res_Des_Handle(resDescPrev);
            if CM_Get_Res_Des_Data_Size(@size, resDesc, 0) = CR_SUCCESS then
            begin
              if size > 0 then
              begin
                if size > resDescBufferSize then
                begin
                  ReallocMem(resDescBuffer, size);
                  resDescBufferSize := size;
                end;
                if CM_Get_Res_Des_Data(resDesc, resDescBuffer, size, 0) = CR_SUCCESS then
                  extractResourceDesc(port, resId, resDescBuffer, size);
              end;
            end;
            resDescPrev := resDesc;
          until CM_Get_Next_Res_Des(@resDesc, resDesc, ResType_All, @resId, 0) <> CR_SUCCESS;
          if resDescPrev <> INVALID_HANDLE_VALUE then
            CM_Free_Res_Des_Handle(resDescPrev);
        finally
          FreeMem(resDescBuffer);
        end;
      end;
      CM_Free_Log_Conf_Handle(logConf);
    end
  end;
end;

// preferred, but does not work under Windows NT
function getPortsWDM(var ports: array of TDetectedPort; guid1: TGUID; portPrefix: string): Boolean;
var
  i, n: Integer;
  devInfoList: HDEVINFO;
  devInfoData: SP_DEVINFO_DATA;
  buffer: array[0..255] of Char;
  instanceId, friendlyName, portName: string;
  key: HKEY;
  type1, size: DWORD;
begin
  Result := False;
  if (@SetupDiClassGuidsFromNameA <> nil)
     and
     (@SetupDiGetClassDevsA <> nil)
     and
     (@SetupDiDestroyDeviceInfoList <> nil)
     and
     (@SetupDiEnumDeviceInfo <> nil)
     and
     (@SetupDiEnumDeviceInterfaces <> nil)
     and
     (@SetupDiGetDeviceInterfaceDetailA <> nil)
     and
     (@SetupDiGetDeviceInstanceIdA <> nil)
     and
     (@SetupDiGetDeviceRegistryPropertyA <> nil)
     and
     (@SetupDiOpenClassRegKeyExA <> nil)
     and
     (@SetupDiOpenDevRegKey <> nil) then
  begin
    devInfoList :=
      SetupDiGetClassDevsA(
        @guid1,
        nil,
        0,
        DIGCF_PRESENT
      );
    if devInfoList <> INVALID_HANDLE_VALUE then
    begin
      devInfoData.cbSize := sizeof(SP_DEVINFO_DATA);
      i := 0;
      while SetupDiEnumDeviceInfo(devInfoList, i, @devInfoData) do
      begin
        if SetupDiGetDeviceInstanceIdA(devInfoList, @devInfoData, @buffer, sizeof(buffer), nil) then
        begin
          instanceId := buffer;
          if SetupDiGetDeviceRegistryPropertyA(devInfoList, @devInfoData, SPDRP_FRIENDLYNAME, nil, @buffer, sizeof(buffer), nil) then
            friendlyName := buffer
          else if SetupDiGetDeviceRegistryPropertyA(devInfoList, @devInfoData, SPDRP_DEVICEDESC, nil, @buffer, sizeof(buffer), nil) then
            friendlyName := buffer
          else
            friendlyName := '';
          // retrieve port name from registry, for example: LPT1, LPT2, COM1, COM2
          key :=
            SetupDiOpenDevRegKey(
              devInfoList,
              @devInfoData,
              DICS_FLAG_GLOBAL,
              0,
              DIREG_DEV,
              KEY_READ
            );
          if key <> INVALID_HANDLE_VALUE then
          begin
            size := 255;
            if RegQueryValueEx(key, 'PortName', nil, @type1, @buffer, @size) = ERROR_SUCCESS then
            begin
              buffer[size] := #0;
              portName := buffer;
              RegCloseKey(key);
            end;
            if (Copy(portName, 1, Length(portPrefix)) = portPrefix) then
            begin
              n := StrToIntDef(Copy(portName, Length(portPrefix) + 1, Length(portName)), 0) - 1;
              if (n >= 0) and (n < Length(ports)) then
              begin
                Result := True;
                if ports[n].friendlyName = '' then
                  if friendlyName <> '' then
                    ports[n].friendlyName := friendlyName
                  else
                    friendlyName := portName;
                // retrieve port hardware address
                getPortResourcesWDMConfigManager(ports[n], devInfoData.DevInst);
              end;
            end;
          end;
        end;
        Inc(i);
      end;
      SetupDiDestroyDeviceInfoList(devInfoList);
    end;
  end;
end;

const
  CmResourceTypeNull           = 0;
  CmResourceTypePort           = 1;
  CmResourceTypeInterrupt      = 2;
  CmResourceTypeMemory         = 3;
  CmResourceTypeDma            = 4;
  CmResourceTypeDeviceSpecific = 5;
  CmResourceTypeBusNumber      = 6;
  CmResourceTypeMaximum        = 7;
  CmResourceTypeDevicePrivate  = 129;

type
  USHORT = Word;
  ULONG = LongWord;
  PHYSICAL_ADDRESS = Int64;
  INTERFACE_TYPE =
  (
        InterfaceTypeUndefined = -1,
        Internal, // = 0
        Isa, // = 1
        Eisa, // = 2
        MicroChannel, // = 3
        TurboChannel, // = 4
        PCIBus, // = 5
        VMEBus, // = 6
        NuBus, // = 7
        PCMCIABus, // = 8
        CBus, // = 9
        MPIBus, // = 10
        MPSABus, // = 11
        ProcessorInternal, // = 12
        InternalPowerBus, // = 13
        PNPISABus, // = 14
        PNPBus, // = 15
        MaximumInterfaceType // = 16
  );
  {$ALIGN 4}
  CM_PARTIAL_RESOURCE_DESCRIPTOR = record
    Type1: UCHAR;
    ShareDisposition: UCHAR;
    Flags: Word;
    case Integer of
      0:
      (
        Generic:
          record
            Start: PHYSICAL_ADDRESS;
            Length: ULONG;
          end;
      );
      1:
      (
        Port:
          record
            Start: PHYSICAL_ADDRESS;
            Length: ULONG;
          end;
      );
      2:
      (
        Interrupt:
          record
            Level: ULONG;
            Vector: ULONG;
            Affinity: ULONG;
          end;
      );
      3:
      (
        Memory:
          record
            Start: PHYSICAL_ADDRESS;
            Length: ULONG;
          end;
      );
      4:
      (
        Dma:
          record
            Channel: ULONG;
            Port: ULONG;
            Reserved1: ULONG;
          end;
      );
      5:
      (
        DevicePrivate:
          record
            Data: array[1..3] of ULONG;
          end;
      );
      6:
      (
        BusNumber:
          record
            Start: ULONG;
            Length: ULONG;
            Reserved: ULONG;
          end;
      );
      7:
      (
        DeviceSpecificData:
          record
            DataSize: ULONG;
            Reserved1: ULONG;
            Reserved2: ULONG;
          end;
      );
  end;
  PCM_PARTIAL_RESOURCE_DESCRIPTOR = ^CM_PARTIAL_RESOURCE_DESCRIPTOR;
  {$ALIGN 8}

  CM_PARTIAL_RESOURCE_LIST = record
    Version: USHORT;
    Revision: USHORT;
    Count: ULONG;
    PartialDescriptors: array of CM_PARTIAL_RESOURCE_DESCRIPTOR;
  end;
  PCM_PARTIAL_RESOURCE_LIST = ^CM_PARTIAL_RESOURCE_LIST;

  CM_FULL_RESOURCE_DESCRIPTOR = record
    InterfaceType: INTERFACE_TYPE;
    BusNumber: ULONG;
    PartialResourceList: CM_PARTIAL_RESOURCE_LIST;
  end;
  PCM_FULL_RESOURCE_DESCRIPTOR = ^CM_FULL_RESOURCE_DESCRIPTOR;

  CM_RESOURCE_LIST = record
    Count: ULONG;
    List: array of CM_FULL_RESOURCE_DESCRIPTOR;
  end;
  PCM_RESOURCE_LIST = ^CM_RESOURCE_LIST;

function fillResourceList(resourceList: PCM_RESOURCE_LIST; buffer: PChar; length: Integer): Boolean;
var
  i, j, p, l: Integer;
begin
  Result := False;
  p := 0;
  l := SizeOf(resourceList^) - SizeOf(resourceList.List);
  if length >= p + l then
  begin
    Move((buffer + p)^, resourceList^, l);
    Inc(p, l);
    SetLength(resourceList.List, resourceList.Count);
    for i := 0 to resourceList.Count - 1 do
    begin
      l := SizeOf(resourceList.List[i]) - SizeOf(resourceList.List[i].PartialResourceList.PartialDescriptors);
      if length >= p + l then
      begin
        Move((buffer + p)^, resourceList.List[i], l);
        Inc(p, l);
        SetLength(resourceList.List[i].PartialResourceList.PartialDescriptors, resourceList.List[i].PartialResourceList.Count);
        for j := 0 to resourceList.List[i].PartialResourceList.Count - 1 do
        begin
          l := SizeOf(resourceList.List[i].PartialResourceList.PartialDescriptors[j]);
          if length >= p + l then
          begin
            Move((buffer + p)^, resourceList.List[i].PartialResourceList.PartialDescriptors[j], l);
            Inc(p, l);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure extractResourceList(var port: TDetectedPort; bChar: PChar; length: Integer);
var
  resourceList: CM_RESOURCE_LIST;
  i, j: Integer;
begin
  if fillResourceList(@resourceList, bChar, length) then
  begin
    for i := 0 to resourceList.Count - 1 do
    begin
      for j := 0 to resourceList.List[i].PartialResourceList.Count - 1 do
      begin
        case resourceList.List[i].PartialResourceList.PartialDescriptors[j].Type1 of
          CmResourceTypePort:
          begin
            if (port.portLength = 0) then
            begin
              // only return the smallest port number from the resource list
              port.portStart := resourceList.List[i].PartialResourceList.PartialDescriptors[j].Port.Start;
              port.portLength := resourceList.List[i].PartialResourceList.PartialDescriptors[j].Port.Length;
            end;
          end;
          CmResourceTypeInterrupt:
          begin
            if not(port.irq) then
            begin
              port.irq := True;
              port.irqLevel := resourceList.List[i].PartialResourceList.PartialDescriptors[j].Interrupt.Level;
            end;
          end;
          CmResourceTypeDma:
          begin
            if not(port.dma) then
            begin
              port.dma := True;
              port.dmaChannel := resourceList.List[i].PartialResourceList.PartialDescriptors[j].Dma.Channel;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure getPortResourcesRegistry(var port: TDetectedPort; key: string; value: string);
var
  bChar: array[0..511] of Char;
  reg: TRegistry;
  l: Integer;
begin
  reg := TRegistry.Create();
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly(key) then
      try
        l := reg.ReadBinaryData(value, bChar, SizeOf(bChar));
        extractResourceList(port, bChar, l);
      finally
        reg.CloseKey();
      end;
  finally
    reg.Free();
  end;
end;

function getPortsNTLegacy(var ports: array of TDetectedPort; portPrefix: string; keyDevicemap: string; keyResourcemap: string; valueResourcemapNamePrefix: string): Boolean;
var
  bStringListDevices, bStringListResources: TStringList;
  reg, reg2: TRegistry;
  i, j, n: Integer;
  portName, valueResourcemapName: string;
begin
  Result := False;
  reg := TRegistry.Create();
  bStringListDevices := TStringList.Create();
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly(keyDevicemap) then
    begin
      try
        reg.GetValueNames(bStringListDevices);
        for i := 0 to bStringListDevices.Count - 1 do
        begin
          // retrieve port name from registry devicemap
          portName := reg.ReadString(bStringListDevices[i]);
          // strip until last backslash
          while Pos('\', portName) > 0 do
            Delete(portName, 1, Pos('\', portName));
          if Copy(portName, 1, Length(portPrefix)) = portPrefix then
          begin
            n := StrToIntDef(Copy(portName, Length(portPrefix) + 1, Length(portName)), 0) - 1;
            if (n >= 0) and (n < Length(ports)) then
            begin
              Result := True;
              if ports[n].friendlyName = '' then
                ports[n].friendlyName := portName;
              // retrieve port hardware address from registry resourcemap
              reg2 := TRegistry.Create();
              bStringListResources := TStringList.Create();
              try
                reg2.RootKey := HKEY_LOCAL_MACHINE;
                if reg2.OpenKeyReadOnly(keyResourcemap) then
                  try
                    reg2.GetKeyNames(bStringListResources);
                  finally
                    reg2.CloseKey();
                  end;
                  valueResourcemapName := valueResourcemapNamePrefix + IntToStr(n) + '.Raw';
                  for j := 0 to bStringListResources.Count - 1 do
                  begin
                    if reg2.OpenKeyReadOnly(keyResourcemap + '\' + bStringListResources[j]) then
                      try
                        if reg2.ValueExists(valueResourcemapName) then
                          getPortResourcesRegistry(ports[n], keyResourcemap + '\' + bStringListResources[j], valueResourcemapName);
                      finally
                        reg2.CloseKey();
                      end;
                  end;
              finally
                bStringListResources.Free();
                reg2.Free();
              end;
            end;
          end;
        end;
      finally
        reg.CloseKey();
      end;
    end;
  finally
    bStringListDevices.Free();
    reg.Free();
  end;
end;

function getPortNTRegistry(var ports: array of TDetectedPort; hardwareId: string): Boolean;
var
  reg: TRegistry;
  n: Integer;
  portName, friendlyName: string;
begin
  Result := False;
  reg := TRegistry.Create();
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    // retrieve port name from registry, for example: LPT1, LPT2, COM1, COM2
    if reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Enum\' + hardwareId + '\Device Parameters') then
    begin
      try
        portName := reg.ReadString('PortName');
        n := StrToIntDef(Copy(portName, 4, Length(portName)), 0) - 1;
      finally
        reg.CloseKey();
      end;
      if (n >= 0) and (n < Length(ports)) then
      begin
        if reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Enum\' + hardwareId) then
          try
            friendlyName := reg.ReadString('FriendlyName');
          finally
            reg.CloseKey();
          end;
        Result := True;
        if ports[n].friendlyName = '' then
          if friendlyName <> '' then
            ports[n].friendlyName := friendlyName
          else
            friendlyName := portName;
        // retrieve port hardware address from registry
        getPortResourcesRegistry(ports[n], 'SYSTEM\CurrentControlSet\Enum\' + hardwareId + '\Control', 'AllocConfig');
      end;
    end;
  finally
    reg.Free();
  end;
end;

function getPortsNT(var ports: array of TDetectedPort; service: string; portPrefix: string; var legacyFound: Boolean): Boolean;
var
  reg: TRegistry;
  i, c: Integer;
  hardwareId: string;
begin
  Result := False;
  legacyFound := False;
  reg := TRegistry.Create();
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    // find all port hardware instance IDs in service, for example:
    // ACPI\PNP0401\4&c575acb&0
    // Root\LEGACY_PARPORT\0000
    // Root\LEGACY_SERIAL\0000
    if reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\' + service + '\Enum') then
    begin
      try
        try
          c := reg.ReadInteger('Count');
        except
          on ERegistryException do
            c := 0;
        end;
        for i := 0 to c - 1 do
        begin
          hardwareId := reg.ReadString(IntToStr(i));
          if Pos('LEGACY_PARPORT', hardwareId) > 0 then
          begin
            legacyFound :=
              getPortsNTLegacy(
                ports,
                portPrefix,
                'HARDWARE\DEVICEMAP\PARALLEL PORTS',
                'HARDWARE\RESOURCEMAP\LOADED PARALLEL DRIVER RESOURCES',
                '\Device\ParallelPort'
              ) or legacyFound;
            Result := Result or legacyFound;
          end
          else if Pos('LEGACY_SERIAL', hardwareId) > 0 then
          begin
            legacyFound :=
              getPortsNTLegacy(
                ports,
                portPrefix,
                'HARDWARE\DEVICEMAP\SERIALCOMM',
                'HARDWARE\RESOURCEMAP\LOADED SERIAL DRIVER RESOURCES',
                '\Device\Serial'
              ) or Result;
            Result := Result or legacyFound;
          end
          else
            Result := getPortNTRegistry(ports, hardwareId) or Result;
        end;
      finally
        reg.CloseKey();
      end;
    end;
  finally
    reg.Free();
  end;
end;

// does not support reporting of hardware resources
// and cannot check if port is still available
function getPorts95(var ports: array of TDetectedPort; portPrefix: string): Boolean;
var
  i, n: Integer;
  portInfo: array[0..255] of PORT_INFO_2;
  size, count: DWORD;
  portName: string;
begin
  Result := False;
  if EnumPorts(nil, 2, @portInfo, sizeof(portInfo), size, count) then
  begin
    for i := 0 to count - 1 do
    begin
      portName := portInfo[i].pPortName;
      while (Length(portName) > 0)
            and
            (not(portName[Length(portName)] in ['0','1','2','3','4','5','6','7','8','9'])) do
      begin
        Delete(portName, Length(portName), 1);
      end;
      if (Copy(portName, 1, Length(portPrefix)) = portPrefix) then
      begin
        n := StrToIntDef(Copy(portName, Length(portPrefix) + 1, Length(portName)), 0) - 1;
        if (n >= 0) and (n < Length(ports)) then
        begin
          Result := True;
          if ports[n].friendlyName = '' then
            ports[n].friendlyName := portInfo[i].pDescription + ' (' + portName + ')';
        end;
      end;
    end;
  end;
end;

function getLPTPorts95(var ports: array of TDetectedPort): Boolean;
var
  n:    Integer;
  l:    DWORD;
  data: array[0..2] of Word;
begin
  Result := False;
  // the LPT4 value from the BIOS is not used, because it could be invalid
  if @toolhelp32ReadProcessMemory <> nil then
  begin
    if toolhelp32ReadProcessMemory(0, Pointer($0408), data, 3 * sizeof(Word), l) then
    begin
      Result := True;
      n := 0;
      while (n < Integer(l div sizeof(Word))) and (n < Length(ports)) do
      begin
        if data[n] > 0 then
        begin
          if ports[n].friendlyName = '' then
            ports[n].friendlyName := 'LPT' + IntToStr(n + 1);
          ports[n].portStart := data[n];
          ports[n].portLength := 3;
        end;
        Inc(n);
      end;
    end;
  end;
end;

initialization
  // dynamically load all optional libraries
  @toolhelp32ReadProcessMemory := nil;
  @SetupDiClassGuidsFromNameA := nil;
  @SetupDiGetClassDevsA := nil;
  @SetupDiDestroyDeviceInfoList := nil;
  @SetupDiEnumDeviceInfo := nil;
  @SetupDiEnumDeviceInterfaces := nil;
  @SetupDiGetDeviceInterfaceDetailA := nil;
  @SetupDiGetDeviceInstanceIdA := nil;
  @SetupDiGetDeviceRegistryPropertyA := nil;
  @SetupDiOpenClassRegKeyExA := nil;
  @SetupDiOpenDevRegKey := nil;
  @CM_Get_First_Log_Conf := nil;
  @CM_Free_Log_Conf_Handle := nil;
  @CM_Get_Next_Res_Des := nil;
  @CM_Free_Res_Des_Handle := nil;
  @CM_Get_Res_Des_Data_Size := nil;
  @CM_Get_Res_Des_Data := nil;
  hKernel32 := LoadLibrary('kernel32.dll');
  if hKernel32 <> 0 then
    @toolhelp32ReadProcessMemory := GetProcAddress(hKernel32, 'Toolhelp32ReadProcessMemory');
  hSetupapi := LoadLibrary('setupapi.dll');
  if hSetupapi <> 0 then
  begin
    @SetupDiClassGuidsFromNameA := GetProcAddress(hSetupapi, 'SetupDiClassGuidsFromNameA');
    @SetupDiGetClassDevsA := GetProcAddress(hSetupapi, 'SetupDiGetClassDevsA');
    @SetupDiDestroyDeviceInfoList := GetProcAddress(hSetupapi, 'SetupDiDestroyDeviceInfoList');
    @SetupDiEnumDeviceInfo := GetProcAddress(hSetupapi, 'SetupDiEnumDeviceInfo');
    @SetupDiEnumDeviceInterfaces := GetProcAddress(hSetupapi, 'SetupDiEnumDeviceInterfaces');
    @SetupDiGetDeviceInterfaceDetailA := GetProcAddress(hSetupapi, 'SetupDiGetDeviceInterfaceDetailA');
    @SetupDiGetDeviceInstanceIdA := GetProcAddress(hSetupapi, 'SetupDiGetDeviceInstanceIdA');
    @SetupDiGetDeviceRegistryPropertyA := GetProcAddress(hSetupapi, 'SetupDiGetDeviceRegistryPropertyA');
    @SetupDiOpenClassRegKeyExA := GetProcAddress(hSetupapi, 'SetupDiOpenClassRegKeyExA'); 
    @SetupDiOpenDevRegKey := GetProcAddress(hSetupapi, 'SetupDiOpenDevRegKey');
  end;
  hCfgmgr32 := LoadLibrary('cfgmgr32.dll');
  if hCfgmgr32 <> 0 then
  begin
    @CM_Get_First_Log_Conf := GetProcAddress(hCfgmgr32, 'CM_Get_First_Log_Conf');
    @CM_Free_Log_Conf_Handle := GetProcAddress(hCfgmgr32, 'CM_Free_Log_Conf_Handle');
    @CM_Get_Next_Res_Des := GetProcAddress(hCfgmgr32, 'CM_Get_Next_Res_Des');
    @CM_Free_Res_Des_Handle := GetProcAddress(hCfgmgr32, 'CM_Free_Res_Des_Handle');
    @CM_Get_Res_Des_Data_Size := GetProcAddress(hCfgmgr32, 'CM_Get_Res_Des_Data_Size');
    @CM_Get_Res_Des_Data := GetProcAddress(hCfgmgr32, 'CM_Get_Res_Des_Data');
  end;
finalization
  FreeLibrary(hCfgmgr32);
  FreeLibrary(hSetupapi);
  FreeLibrary(hKernel32);
end.

