{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module System.Win32.Dpi
  ( withDpiFunctions
  , DpiFunctions(..)
  , getNonClientMetricsForDpi
  , adjustWindowRectExForDpi
  , getDpiForSystem_
  , getDpiForWindow_
  , getSystemMetricsForDpi_
  ) where

import Prelude hiding (putStrLn, print)
import Control.Monad
import Control.Exception
import System.Win32 hiding (getProcAddress)
import Graphics.Win32.GDI.Types
import System.Win32.Info.Parameters
import System.Win32.Info
import Foreign
import Foreign.C.String
import Data.Bool
import Graphics.Win32

data DpiFunctions = DpiFunctions
  { getDpiForSystem :: Maybe (IO UINT)
  , getDpiForWindow :: Maybe (HWND -> IO UINT)
  , c_AdjustWindowRectExForDpi 
     :: Maybe ( LPRECT 
               -> DWORD 
               -> BOOL 
               -> DWORD 
               -> UINT 
               -> IO BOOL)
  , systemParametersInfoForDpi 
     :: Maybe (SPIUIAction 
               -> UINT
               -> LPVOID
               -> UINT 
               -> UINT
               -> IO BOOL)
  , getSystemMetricsForDpi :: Maybe (SMSetting -> UINT -> IO INT)
  }

withDpiFunctions :: (DpiFunctions -> IO a) -> IO a
#ifdef DYNAMIC
withDpiFunctions act = 
  bracket 
    (loadLibrary "User32.dll") 
    (freeLibrary)
    (act <=< getDpiFunctions)

getProcAddress :: HMODULE -> String -> IO (Maybe Addr)
getProcAddress hmod procname = do
  p <- withCAString procname $ \ c_procname ->
    c_GetProcAddress hmod c_procname
  return $ bool (Just p) Nothing (p == nullPtr)

getDpiFunctions :: HMODULE -> IO DpiFunctions
getDpiFunctions user32dll = do
  fpGetDpiForSystem            <- getProcAddress user32dll "GetDpiForSystem"
  fpGetDpiForWindow            <- getProcAddress user32dll "GetDpiForWindow" 
  fpSystemParametersInfoForDpi <- getProcAddress user32dll "SystemParametersInfoForDpi"
  fpAdjustWindowRectExForDpi   <- getProcAddress user32dll "AdjustWindowRectExForDpi"
  fpGetSystemMetricsForDpi     <- getProcAddress user32dll "GetSystemMetricsForDpi"


  let getDpiForSystem = fmap (callGetDpiForSystem . castPtrToFunPtr) fpGetDpiForSystem
  let getSystemMetricsForDpi = fmap (callGetSystemMetricsForDpi . castPtrToFunPtr) fpGetSystemMetricsForDpi
  let getDpiForWindow = fmap (callGetDpiForWindow . castPtrToFunPtr) fpGetDpiForWindow
  let systemParametersInfoForDpi = fmap (callSystemParametersInfoForDpi . castPtrToFunPtr) fpSystemParametersInfoForDpi
  let c_AdjustWindowRectExForDpi = fmap (callAdjustWindowRectExForDpi . castPtrToFunPtr) fpAdjustWindowRectExForDpi
  let out = DpiFunctions {..}
  return out

foreign import ccall "dynamic" 
  callGetSystemMetricsForDpi :: FunPtr (SMSetting -> UINT -> IO INT) -> SMSetting -> UINT -> IO INT
foreign import ccall "dynamic" 
  callGetDpiForSystem :: FunPtr (IO UINT) -> IO UINT
foreign import ccall "dynamic" 
  callGetDpiForWindow :: FunPtr (HWND -> IO UINT) -> HWND -> IO UINT
foreign import ccall "dynamic" 
  callSystemParametersInfoForDpi 
  :: FunPtr (SPIUIAction -> UINT -> LPVOID -> UINT -> UINT -> IO BOOL)
  -> SPIUIAction -> UINT -> LPVOID -> UINT -> UINT -> IO BOOL
foreign import ccall "dynamic"
  callAdjustWindowRectExForDpi
  :: FunPtr (LPRECT -> DWORD -> BOOL -> DWORD -> UINT -> IO BOOL)
  -> LPRECT -> DWORD -> BOOL -> DWORD -> UINT -> IO BOOL
 
 

#else
withDpiFunctions act = act DpiFunctions
  { getDpiForSystem = Just c_GetDpiForSystem
  , getDpiForWindow = Just c_GetDpiForWindow
  , systemParametersInfoForDpi = Just c_SystemParametersInfoForDpi
  , c_AdjustWindowRectExForDpi = Just c_AdjustWindowRectExForDpi'
  , getSystemMetricsForDpi = Just c_GetSystemMetricsForDpi
  }

foreign import ccall "GetDpiForSystem" 
  c_GetDpiForSystem :: IO UINT
foreign import ccall "GetDpiForWindow" 
  c_GetDpiForWindow :: HWND -> IO UINT

foreign import ccall "GetSystemMetricsForDpi" 
  c_GetSystemMetricsForDpi :: SMSetting -> UINT -> IO INT

foreign import ccall "SystemParametersInfoForDpi" 
  c_SystemParametersInfoForDpi 
  :: SPIUIAction -> UINT -> LPVOID -> UINT -> UINT -> IO BOOL

foreign import ccall "windows.h AdjustWindowRectExForDpi" 
  c_AdjustWindowRectExForDpi' 
  :: LPRECT -> DWORD -> BOOL -> DWORD -> UINT -> IO BOOL

error ! error

#endif

getNonClientMetricsForDpi :: DpiFunctions -> UINT -> IO NONCLIENTMETRICSW
getNonClientMetricsForDpi fns dpi =
  getNonClientMetricsWithClosure 
     (\action uiParam pvParam fWinIni 
         -> systemParametersInfoForDpi_ fns action uiParam pvParam fWinIni dpi)

systemParametersInfoForDpi_ :: DpiFunctions -> SPIUIAction -> UINT -> LPVOID -> UINT -> UINT -> IO BOOL
systemParametersInfoForDpi_ fns action uiParam pvParam fWinIni dpi =
   maybe (systemParametersInfo action uiParam pvParam fWinIni) (\f -> f action uiParam pvParam fWinIni dpi) 
     (systemParametersInfoForDpi fns)

adjustWindowRectExForDpi :: DpiFunctions -> RECT -> DWORD -> BOOL -> DWORD -> UINT -> IO RECT
adjustWindowRectExForDpi fns rect dwStyle bMenu dwExStyle dpi = 
  withRECT rect $ \lprect -> do
    failIfFalse_ "AdjustWindowRectExForDpi" $ c_AdjustWindowRectExForDpi_ fns lprect dwStyle bMenu dwExStyle dpi
    peekRECT lprect

c_AdjustWindowRectExForDpi_ :: DpiFunctions -> LPRECT -> DWORD -> BOOL -> DWORD -> UINT -> IO BOOL
c_AdjustWindowRectExForDpi_ fns lprect dwStyle bMenu dwExStyle dpi =
  maybe (c_AdjustWindowRectEx lprect dwStyle bMenu dwExStyle) (\f -> f lprect dwStyle bMenu dwExStyle dpi)
    (c_AdjustWindowRectExForDpi fns)

getDpiForSystem_ :: DpiFunctions -> IO UINT
getDpiForSystem_ fns = 
  maybe (return 96) (id) (getDpiForSystem fns)

getDpiForWindow_ :: DpiFunctions -> HWND -> IO UINT
getDpiForWindow_ fns hwnd = 
  maybe (return 96) ($ hwnd) (getDpiForWindow fns)

foreign import ccall "SystemParametersInfoW" 
  systemParametersInfo
  :: SPIUIAction -> UINT -> LPVOID -> UINT -> IO BOOL

getSystemMetricsForDpi_ :: DpiFunctions -> SMSetting -> UINT -> IO INT
getSystemMetricsForDpi_ fns setting dpi = 
  maybe (c_GetSystemMetrics setting) (\f -> f setting dpi) 
    (getSystemMetricsForDpi fns)

foreign import ccall "GetSystemMetrics" 
  c_GetSystemMetrics :: SMSetting -> IO INT
