# win32-dpi
Utilities for Win32 HiDPI work in haskell

These functions are not exposed in the version of Mingw w64 that GHC 8.8.3 uses. 
You can link statically if you copy the relevant .lib file into GHC's Mingw.
Otherwise it links dynamically by default so you just need an adequately recent windows.
