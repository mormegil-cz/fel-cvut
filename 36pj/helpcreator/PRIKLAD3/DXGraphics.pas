unit DXGraphics;

// FIXME: The "enumerated" types are named quite randomly (TD3DSwapEffect ..-> TD3DSwappingEffect, etc)
//        maybe some _ tail, or something could help

interface

{$DEFINE ALLOW_PROP_CHANGE}  { If defined, some properties could be changed even though they
                               have no effect until Reset.
                               If not defined, such attempt throws an exception. }

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DirectXGraphics, D3DX8;

type
  EDXGraphics = class(Exception);      // the only exception class raised by DXGraphics

  // forward declarations
  TDirectXAdapter = class;
  TDirectXAdapterMode = class;
  TDirectXGraphics = class;
  TDirect3DSurface = class;
  TDirect3DDevice = class;
  TDeviceStateBlock = class;

  // event types
  TChooseAdapterEvent = procedure(Sender: TDirectXGraphics) of object;
  TChooseModeEvent = procedure(Sender: TDirectXGraphics) of object;
  TConfirmModeEvent = function(const Mode: TDirectXAdapterMode):Boolean of object;

  // other property types

  // enumerated
  TDepthBufferKind = (dbNone, dbZBuffer, dbWBuffer);

  // "enumerated"
  TDXRefreshRate = type UINT;                    // refresh rate in Hz, or D3DPRESENT_RATE_xxx
  TDXPresentationInterval = type Integer;        // one from D3DPRESENT_INTERVAL_xxx constants
  TDXDisplayFormat = type Integer;               // one from D3DFMT_xxx constants
  TD3DDeviceType = type TD3DDevType;             // one from D3DDEVTYPE_xxx constants
  TD3DSwappingEffect = type TD3DSwapEffect;      // one from D3DSWAPEFFECT_xxx constants
  TD3DMultiSampling = type TD3DMultiSample_Type; // one from D3DMULTISAMPLE_xxx constants

  // options
  TDirectXGraphicsOption = (
                         // basic
                         doFullScreen, doAutoReset,
                         // device creation
                         doPreserveFPU, doMultithreaded, doPureDevice, doLockableBackBuffer,
                         // adapter/mode enumeration
                         doEnumAdapters, doEnumAdapterModes, doCheckAdapterWHQL
  );
  TDirectXGraphicsOptions = set of TDirectXGraphicsOption;

  // ============= TDXDisplayMode ================
  TDXDisplayMode = class(TPersistent)
  private
    FIndexed: Boolean;
    FRefresh: TDXRefreshRate;
    FBPP: UINT;
    FHeight: UINT;
    FWidth: UINT;
    FAlphaBits: UINT;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
  published
    property Width:     UINT           read FWidth     write FWidth     default 800;
    property Height:    UINT           read FHeight    write FHeight    default 600;
    property BPP:       UINT           read FBPP       write FBPP       default 24;
    property AlphaBits: UINT           read FAlphaBits write FAlphaBits default 0;
    property Indexed:   Boolean        read FIndexed   write FIndexed   default False;  // indexed color (palette)
    property Refresh:   TDXRefreshRate read FRefresh   write FRefresh   default D3DPRESENT_RATE_DEFAULT;
  end;

  // ============= TDirectXAdapter ================
  TDirectXAdapter = class
  private
    FDesktopMode: TD3DDisplayMode;
  protected
    FHandle: UINT;
    FParentDXG: TDirectXGraphics;
    FMonitor: HMonitor;
    FIdentifier: TD3DAdapter_Identifier8;
    FModes: TList;

    FCaps: TD3DCaps8;
  public
    constructor Create(const DXGraphics: TDirectXGraphics; const Adapter: UINT);
    destructor  Destroy; override;

    property ParentDXG: TDirectXGraphics         read FParentDXG;
    property Handle: UINT                        read FHandle;
    property Identifier: TD3DAdapter_Identifier8 read FIdentifier;
    property Monitor: HMonitor                   read FMonitor;
    property Caps: TD3DCaps8                     read FCaps;
    property DesktopMode: TD3DDisplayMode        read FDesktopMode;

    property Modes: TList                        read FModes;
  end;

  // ============= TDirectXAdapterMode ================
  TDirectXAdapterMode = class
  private
    FAdapter: TDirectXAdapter;
    FFormat: TD3DFormat;
    FHeight: UINT;
    FOrdinal: UINT;
    FWidth: UINT;
    FBPP: UINT;
    FRefreshRate: UINT;
    FAlphaBits: UINT;
    FIndexed: Boolean;
    FCanBeTarget: Boolean;
    FConfirmed: Boolean;
    FTnLBehavior: DWORD;
    FCanRenderWindowed: Boolean;
    FSupported: Boolean;
    FDepthStencil: TD3DFormat;

    procedure ImportMode(const ModeData: TD3DDisplayMode);
    procedure FindDepthStencil;
  public
    constructor Create(const IAdapter: TDirectXAdapter; const Mode: UINT);
    constructor CreateFromMode(const IAdapter: TDirectXAdapter; const ModeData: TD3DDisplayMode);

    property Adapter:     TDirectXAdapter read FAdapter;
    property Ordinal:     UINT            read FOrdinal;
    property Width:       UINT            read FWidth;
    property Height:      UINT            read FHeight;
    property BPP:         UINT            read FBPP;
    property AlphaBits:   UINT            read FAlphaBits;
    property Indexed:     Boolean         read FIndexed;      // indexed color (palette)
    property RefreshRate: UINT            read FRefreshRate;
    property Format:      TD3DFormat      read FFormat;

    property Supported:   Boolean         read FSupported;    // this mode is supported
    property CanBeTarget: Boolean         read FCanBeTarget;  // can be used as render target
    property CanRenderWindowed: Boolean   read FCanRenderWindowed;      // can run in a window in this mode

    property Confirmed:   Boolean         read FConfirmed;    // can process vertices
    property TnLBehavior: DWORD           read FTnLBehavior;  // vertex processing flags

    property DepthStencil: TD3DFormat     read FDepthStencil; // best depth/stencil buffer format
  end;

  // ============= TDirectXGraphics ================
  TDirectXGraphics = class(TComponent)
  private
    ParentControl: TWinControl;
    Initialized: Boolean;
    ResetLock: Boolean;        // semaphore for the Reset method

    { containers for properties and their access methods }
    FD3D8: IDirect3D8;
    FDevice: TDirect3DDevice;
    FSurface: TDirect3DSurface;
    FDesktopMode: TD3DDisplayMode;
    FOptions: TDirectXGraphicsOptions;
    FOnChooseAdapter: TChooseAdapterEvent;
    FAdapter: UINT;
    FDeviceType: TD3DDeviceType;
    FVertexProcessing: DWORD;
    FAdapters: TList;
    FDesiredMode: TDXDisplayMode;
    FDisplayFormat: TDXDisplayFormat;
    FOnChooseMode: TChooseModeEvent;
    FDisplayRefresh: TDXRefreshRate;
    FBackBuffers: Integer;
    FPresentationInterval: TDXPresentationInterval;
    FSwapEffect: TD3DSwappingEffect;
    FMultiSample: TD3DMultiSampling;
    FDepthBufferFormat: TDXDisplayFormat;
    FUseDepthBuffer: TDepthBufferKind;
    FOnConfirmMode: TConfirmModeEvent;
    FMinStencilBits: Integer;
    FMinDepthBits: Integer;
    FOnPreReset: TNotifyEvent;
    FOnPostReset: TNotifyEvent;
    FCanDraw: Boolean;
    FInControl: TWinControl;
    procedure SetOptions(const Value: TDirectXGraphicsOptions);
    procedure SetAdapter(const Value: UINT);
    procedure SetDeviceType(const Value: TD3DDeviceType);
    procedure SetVertexProcessing(const Value: DWORD);
    procedure SetDesiredMode(const Value: TDXDisplayMode);
    procedure SetDisplayFormat(const Value: TDXDisplayFormat);
    procedure SetDisplayRefresh(const Value: TDXRefreshRate);
    procedure SetBackBuffers(const Value: Integer);
    procedure SetPresentationInterval(
      const Value: TDXPresentationInterval);
    procedure SetSwapEffect(const Value: TD3DSwappingEffect);
    procedure SetMultiSample(const Value: TD3DMultiSampling);
    procedure SetDepthBufferFormat(const Value: TDXDisplayFormat);
    procedure SetUseDepthBuffer(const Value: TDepthBufferKind);
    procedure SetMinDepthBits(const Value: Integer);
    procedure SetMinStencilBits(const Value: Integer);
    procedure SetInControl(const Value: TWinControl);
  protected
  public
    { --- methods --- }
    { basics }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { elementary }
    procedure   Initialize;
    procedure   Finalize;

    function    Reset: Boolean;           // true = success, false = device lost

    { page flipping }

    { --- properties --- }
    { elementary }
    property Surface: TDirect3DSurface read FSurface;

    property D3D: IDirect3D8  read FD3D8;
    property D3D8: IDirect3D8 read FD3D8;

    property Adapter: UINT               read FAdapter write SetAdapter;
    property Device:  TDirect3DDevice    read FDevice;

    property CanDraw: Boolean            read FCanDraw;

    property DesktopMode: TD3DDisplayMode read FDesktopMode;

    { really used surface formats }
    property DepthBufferFormat: TDXDisplayFormat read FDepthBufferFormat write SetDepthBufferFormat;
    property DisplayFormat:     TDXDisplayFormat read FDisplayFormat     write SetDisplayFormat;
    property DisplayRefresh:    TDXRefreshRate   read FDisplayRefresh    write SetDisplayRefresh;

    { chosen vertex processing flags }
    property VertexProcessing: DWORD read FVertexProcessing write SetVertexProcessing;

    { enumerated lists }
    property Adapters: TList read FAdapters;

    { page flipping }
     // management of swap chains, etc.

  published
    // control embedding
    property InControl: TWinControl          read FInControl      write SetInControl;

    // chosen display mode to use
    property DesiredMode: TDXDisplayMode     read FDesiredMode    write SetDesiredMode;

    // how many back buffers (0 is considered 1)
    property BackBuffers: Integer            read FBackBuffers    write SetBackBuffers    default 1;

    // requirements on depth/stencil buffer
    property MinDepthBits:      Integer          read FMinDepthBits   write SetMinDepthBits   default 16;
    property MinStencilBits:    Integer          read FMinStencilBits write SetMinStencilBits default 0;
    property UseDepthBuffer:    TDepthBufferKind read FUseDepthBuffer write SetUseDepthBuffer default dbZBuffer;

    // presentation parameters
    property PresentationInterval: TDXPresentationInterval read FPresentationInterval write SetPresentationInterval default D3DPRESENT_INTERVAL_DEFAULT;
    property SwapEffect: TD3DSwappingEffect read FSwapEffect write SetSwapEffect default D3DSWAPEFFECT_DISCARD;
    property MultiSample: TD3DMultiSampling read FMultiSample write SetMultiSample default D3DMULTISAMPLE_NONE;

    // device parameters
    property DeviceType: TD3DDeviceType read FDeviceType write SetDeviceType default D3DDEVTYPE_HAL;

    // options
    property Options: TDirectXGraphicsOptions read FOptions write SetOptions default [doAutoReset];

    // events
    property OnPreReset:  TNotifyEvent read FOnPreReset  write FOnPreReset;
    property OnPostReset: TNotifyEvent read FOnPostReset write FOnPostReset;

    property OnChooseAdapter: TChooseAdapterEvent read FOnChooseAdapter write FOnChooseAdapter;
    property OnChooseMode: TChooseModeEvent read FOnChooseMode write FOnChooseMode;
    property OnConfirmMode: TConfirmModeEvent read FOnConfirmMode write FOnConfirmMode;
  end;

  TSurfaceOrigin = (soUnknown, soSurface, soImage, soDepthStencil, soNullSurface);
  // ============= TDirect3DSurface ================
  TDirect3DSurface = class
  private
    ParentDev: TDirect3DDevice;
    FSurface:  IDirect3DSurface8;
    FDesc:     TD3DSurface_Desc;
    Origin:    TSurfaceOrigin;
  protected
  public
    { --- methods --- }
    { elementary }
    constructor CreateFromSurface(const Device: TDirect3DDevice; const Source: IDirect3DSurface8);
    constructor CreateImageSurface(const Device: TDirect3DDevice; const Width, Height: UINT; const Format: TD3DFormat);
    constructor CreateDepthStencilSurface(const Device: TDirect3DDevice; const Width, Height: UINT;
                                          const Format: TD3DFormat; const MultiSample: TD3DMultiSample_Type);
    constructor CreateNullSurface(const Device: TDirect3DDevice);

    destructor  Destroy; override;

    procedure   Recreate;         // (after Release)
    procedure   Release;

    { --- properties --- }
    { elementary }
    property Surface: IDirect3DSurface8 read FSurface;
    property Surface8: IDirect3DSurface8 read FSurface;

    property Desc: TD3DSurface_Desc read FDesc;
  published
  end;

  // ============= TDirect3DDevice ================
  TDirect3DDevice = class
  private
    ParentDXG: TDirectXGraphics;
    ResetLock: Boolean;            // semaphore for the Reset method

    { containers for properties and their access methods }
    FDevice8: IDirect3DDevice8;
    FMouseCursor: TDirect3DSurface;
    FMouseHotSpot: TPoint;
    FDeviceStates: TDeviceStateBlock;
    FCaps: TD3DCaps8;
    FSurface: TDirect3DSurface;
    FZBuffer: TDirect3DSurface;
    procedure SetMouseCursor(const Value: TDirect3DSurface);
    procedure SetMouseHotSpot(const Value: TPoint);
  protected
    HasZBuffer: Boolean;
    AssociatedSurfaces: TList;
  public
    { --- methods --- }
    { basics }
    constructor Create(const Parent: TDirectXGraphics);
    destructor  Destroy; override;

    { elementary }
    function  Reset: Boolean;           // true = success, false = device lost
    function  CanBeReset: Boolean;

    { surface }
    procedure Clear(const Color: TD3DColor = 0; const Z: Single = 1.0); // clear target (and zbuffer, if present)
    procedure ClearT(const Color: TD3DColor);                           // clear target only
    procedure ClearTZ(const Color: TD3DColor; const Z: Single);         // clear target and zbuffer
    procedure ClearTZS(const Color: TD3DColor; const Z: Single; const Stencil: DWORD);  // clear target, zbuffer and stencil buffer

    { basic 2d primitives }
    procedure Line(const X1, Y1, X2, Y2: Integer; const Color: TD3DColor);
    procedure Rectangle(const X1, Y1, X2, Y2: Integer; const Color: TD3DColor);
    procedure FillRect(const X1, Y1, X2, Y2: Integer; const Color: TD3DColor);

    { surface management }
    procedure RegisterAssociatedSurface(const Surface: TDirect3DSurface);
    procedure UnRegisterAssociatedSurface(const Surface: TDirect3DSurface);

    { page flipping }
    procedure Present;

    { --- properties --- }
    { elementary }
    property Device: IDirect3DDevice8 read FDevice8;
    property Device8: IDirect3DDevice8 read FDevice8;

    property Surface: TDirect3DSurface read FSurface;      // back surface
    property ZBuffer: TDirect3DSurface read FZBuffer;      // depth-stencil buffer

    { device-state block }
    property DeviceStates: TDeviceStateBlock read FDeviceStates;

    { device capabilities }
    property Caps: TD3DCaps8 read FCaps;

    { mouse cursor }
    property MouseHotSpot: TPoint read FMouseHotSpot write SetMouseHotSpot;
    property MouseCursor:  TDirect3DSurface read FMouseCursor write SetMouseCursor;
  published
  end;

  // ============= TDeviceStateBlock ================
  TDeviceStateBlock = class
  private
    IDevice:  IDirect3DDevice8;
    Recording,
    Recorded: Boolean;
    Token:    DWORD;

    { property access methods }
    function GetState(State: TD3DRenderStateType): DWORD;
    procedure SetState(State: TD3DRenderStateType; const Value: DWORD);
  public
    constructor Create(const Device: TDirect3DDevice);
    destructor  Destroy; override;

    procedure   Capture;

    procedure   StartBlock;
    procedure   EndBlock;
    procedure   Apply;

    property    State[State: TD3DRenderStateType]: DWORD read GetState write SetState; default;
  end;

  TResourceUsage = (ruDepthStencil, ruRenderTarget);
  TResourceUsages = set of TResourceUsage;
  // ============ TDirect3DResource
  TDirect3DResource = class
  private
    { property access methods }
    function GetPriority: DWORD;
    procedure SetPriority(const Value: DWORD);
  protected
    FResource: IDirect3DResource8;
  public
    property Resource: IDirect3DResource8 read FResource;
    property Priority: DWORD read GetPriority write SetPriority;
  published
  end;

  // ============ TDirect3DBaseTexture
  TDirect3DBaseTexture = class(TDirect3DResource)
  private
    { property access methods }
    function GetLOD: DWORD;
    procedure SetLOD(const Value: DWORD);
  protected
    FBaseTexture: IDirect3DBaseTexture8;
  public
    property BaseTexture: IDirect3DBaseTexture8 read FBaseTexture;
    property LOD: DWORD read GetLOD write SetLOD;
  published
  end;

  // ============ TDirect3DTexture
  TDirect3DTexture = class(TDirect3DBaseTexture)
  private
    ParentDev: TDirect3DDevice;

    { containers for properties and their access methods }
    FFormat: TD3DFormat;
    FHeight: UINT;
    FWidth: UINT;
    FMipLevels: UINT;
    FStage: DWORD;
  protected
    FTexture: IDirect3DTexture8;
  public
    constructor Create(const Device: TDirect3DDevice;
                       const Width, Height, MIPLevels: UINT;
                       const Format: TD3DFormat;
                       const Usage: TResourceUsages = [];
                       const Pool: TD3DPool = D3DPOOL_MANAGED);
    constructor CreateFromFile(const Device: TDirect3DDevice;
                       const FileName: string;
                       const ColorKey: TD3DColor = 0;
                       const PPalette: PPaletteEntry = nil;
                       const MipLevels: UINT = D3DX_DEFAULT;
                       const Usage: TResourceUsages = [];
                       const Pool: TD3DPool = D3DPOOL_MANAGED;
                       const Filter: DWORD = D3DX_DEFAULT;
                       const MipFilter: DWORD = D3DX_DEFAULT);
    constructor CreateFromFileInMemory(const Device: TDirect3DDevice;
                       const Data: Pointer;
                       const DataSize: UINT;
                       const ColorKey: TD3DColor = 0;
                       const PPalette: PPaletteEntry = nil;
                       const MipLevels: UINT = D3DX_DEFAULT;
                       const Usage: TResourceUsages = [];
                       const Pool: TD3DPool = D3DPOOL_MANAGED;
                       const Filter: DWORD = D3DX_DEFAULT;
                       const MipFilter: DWORD = D3DX_DEFAULT);

    destructor Destroy; override;

    procedure  Activate;
    procedure  Deactivate; overload;

    class procedure Deactivate(const Dev: TDirect3DDevice; const Stage: Integer); overload;

    property Texture: IDirect3DTexture8 read FTexture;
    property Width: UINT read FWidth;
    property Height: UINT read FHeight;
    property Format: TD3DFormat read FFormat;
    property MipLevels: UINT read FMipLevels;

    property Stage: DWORD read FStage write FStage;
  published
  end;

{ Common routines }
procedure DxCheck(const Result: HRESULT);
procedure D3DXCheck(const Result: HRESULT);

implementation

