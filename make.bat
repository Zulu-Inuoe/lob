@setlocal

@set SRC_DIR=%~dp0src\
@set BIN_DIR=%~dp0bin\
@set LOB_ASD=%SRC_DIR%lob\com.inuoe.lob.asd
@set LOB_EXE=%BIN_DIR%lob.exe

@rem Escape backslashes for SBCL string literals
@set LOB_ASD_E=%LOB_ASD:\=\\%
@set LOB_EXE_E=%LOB_EXE:\=\\%

@set BUILD_EXP=^
(if (com.inuoe.lob:build :image \"sbcl\" :loaded-things \"%LOB_ASD_E%\" :output-path \"%LOB_EXE_E%\")^
  (sb-ext:exit :code 0)^
  (sb-ext:exit :code 1))

@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --disable-debugger^
 --eval "(require ""ASDF\"")"^
 --eval "(asdf:load-asd #p""%LOB_ASD_E%\"")"^
 --eval "(asdf:load-system '#:com.inuoe.lob)"^
 --eval "%BUILD_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@set LOB_PROJECT_ASD=%SRC_DIR%lob-project\com.inuoe.lob-project.asd
@set LOB_PROJECT_EXE=%BIN_DIR%lob-project.exe

%LOB_EXE% %LOB_PROJECT_ASD% -o %LOB_PROJECT_EXE% -v
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
