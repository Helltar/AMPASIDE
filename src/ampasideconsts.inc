LE = LineEnding;
DIR_SEP = DirectorySeparator;

APP_NAME = 'AMPASIDE';
APP_VERSION = '1.3';

APP_DIR_CONFIG = 'config' + DIR_SEP;
APP_DIR_HELP = APP_DIR_CONFIG + 'help' + DIR_SEP;
APP_DIR_IMG = 'img' + DIR_SEP;
APP_DIR_LIBS = 'lib' + DIR_SEP;
APP_DIR_STUBS = 'stubs' + DIR_SEP;
APP_DIR_TMP = 'tmp' + DIR_SEP;
APP_DIR_TOOLS = 'bin' + DIR_SEP;
APP_DIR_ANDROID = 'android' + DIR_SEP;
APP_DIR_LOGS = 'log' + DIR_SEP;

ANT_LOG = APP_DIR_LOGS + 'ant.log';
APP_CONFIG = APP_DIR_CONFIG + 'ampaside';
EDITOR_COMPLETION = APP_DIR_CONFIG + 'editorcompletion';
EDITOR_HEADERS = APP_DIR_CONFIG + 'editorheaders';
JCF_SETTINGS = APP_DIR_CONFIG + 'jcfsettings';

CLASS_FW = 'FW.class';

PROJ_DIR_ANDROID = 'apk';
PROJ_DIR_BIN = 'bin';
PROJ_DIR_JAVAME = 'jar';
PROJ_DIR_LIBS = 'libs';
PROJ_DIR_PRE_BUILD = 'pre-build';
PROJ_DIR_RES = 'res';
PROJ_DIR_SRC = 'src';

EXT_APK = '.apk';
EXT_CLASS = '.class';
EXT_JAD = '.jad';
EXT_JAR = '.jar';
EXT_MODULE = '.pas';
EXT_NOTES = '.txt';
EXT_PROJECT = '.aproj';

EMULATOR = APP_DIR_TOOLS + 'emulator' + DIR_SEP +'microemulator.jar';
ANDROID_MANIFEST = 'AndroidManifest.xml';

{$IFDEF UNIX}

  {$IFDEF CPUX86_64}
    A_CPU = 'x86_64';
  {$ENDIF}

  {$IFDEF CPUI386}
    A_CPU = 'i386';
  {$ENDIF}

  FILE_ARCHIVER = APP_DIR_TOOLS + 'linux_' + A_CPU + DIR_SEP + '7za';
  JCF = APP_DIR_TOOLS + 'linux_' + A_CPU + DIR_SEP + 'JCF';
  MP3CC = APP_DIR_TOOLS + 'linux_' + A_CPU + DIR_SEP + 'mp3CC';

{$ENDIF}

{$IFDEF MSWINDOWS}
  FILE_ARCHIVER = APP_DIR_TOOLS + 'win_i386' + DIR_SEP + '7za.exe';
  JCF = APP_DIR_TOOLS + 'win_i386' + DIR_SEP + 'JCF.exe';
  MP3CC = APP_DIR_TOOLS + 'win_i386' + DIR_SEP + 'mp3CC.exe';
{$ENDIF}

URL_JAVA_LIB = 'http://forum.boolean.name/showthread.php?t=5494';
URL_SRC_CODE = 'https://github.com/Helltar/AMPASIDE';
