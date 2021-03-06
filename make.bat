@setlocal

@set SRC_DIR=%~dp0src\
@set BIN_DIR=%~dp0bin\
@set LOB_ASD=%SRC_DIR%com.inuoe.lob.asd
@set LOB_EXE=%BIN_DIR%lob.exe

@rem Escape backslashes for SBCL string literals
@set LOB_ASD_E=%LOB_ASD:\=\\%
@set LOB_EXE_E=%LOB_EXE:\=\\%

@set BUILD_EXP=^
(if (com.inuoe.lob/commands/build:build :image \"sbcl\" :loaded-things \"%LOB_ASD_E%\" :output-path \"%LOB_EXE_E%\" :format-error t)^
  (sb-ext:exit :code 0)^
  (sb-ext:exit :code 1))

@sbcl^
 --noinform^
 --disable-ldb^
 --lose-on-corruption^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --noprint^
 --disable-debugger^
 --load "%SRC_DIR%uiop.lisp"^
 --load "%SRC_DIR%asdf.lisp"^
 --eval "(asdf:load-asd #p""%LOB_ASD_E%\"")"^
 --eval "(asdf:load-system '#:com.inuoe.lob/commands/build)"^
 --eval "%BUILD_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
