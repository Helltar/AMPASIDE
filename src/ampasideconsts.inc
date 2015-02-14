LE = LineEnding;
DIR_SEP = DirectorySeparator;

APP_NAME = 'AMPASIDE';
APP_VERSION = '0.9';

APP_DIR_DATA = 'data' + DIR_SEP;
APP_DIR_IMG = 'img' + DIR_SEP;
APP_DIR_LIBS = 'libs' + DIR_SEP;
APP_DIR_STUBS = 'stubs' + DIR_SEP;
APP_DIR_TMP = 'tmp' + DIR_SEP;
APP_DIR_TOOLS = 'tools' + DIR_SEP;
APP_DIR_ANDROID = APP_DIR_TOOLS + 'android' + DIR_SEP;

ANT_LOG = APP_DIR_DATA + 'ant.log';
APP_CONFIG = APP_DIR_DATA + 'ampaside';
EDITOR_COMPLETION = APP_DIR_DATA + 'editorcompletion';
EDITOR_HEADERS = APP_DIR_DATA + 'editorheaders';
JCF_SETTINGS = APP_DIR_DATA + 'jcfsettings';

CLASS_FW = 'FW.class';

PROJ_DIR_ANDROID = 'android';
PROJ_DIR_BIN = 'bin';
PROJ_DIR_JAVAME = 'javame';
PROJ_DIR_LIBS = 'libs';
PROJ_DIR_PRE_BUILD = 'pre-build';
PROJ_DIR_RES = 'res';
PROJ_DIR_SRC = 'src';

EXT_APK = '.apk';
EXT_CLASS = '.class';
EXT_JAD = '.jad';
EXT_JAR = '.jar';
EXT_MODULE = '.pas';
EXT_NOTES = '.atxt';
EXT_PROJECT = '.aproj';

EMULATOR = APP_DIR_TOOLS + 'microemulator.jar';

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
URL_TUTORIAL = 'http://helltar.ho.ua/mpascal/manual';

