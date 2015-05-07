AMPASIDE
========

Advanced MIDletPascal IDE

Среда разработки использующая компилятор языка программирования [MIDletPascal](http://ru.wikipedia.org/wiki/MIDletPascal).

Установка
---------

### Linux

Выпускаемые версии на **Qt**, установите [Free Pascal Qt4 Binding](http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html):

``` bash
# yum, zypper, etc...
sudo apt-get install libqt4pas-dev
```

(при необходимости под **GTK** вы можете собрать из исходников)

Для запуска эмулятора должна быть установлена **JRE**:

```
java -version
```

#### Для возможности сборки под Android

Должен быть установлен **JDK**:

```
javac -version
```

Установите **Apache Ant**:

```
sudo apt-get install ant
```

##### Linux x86_64

Поскольку некоторые утилиты **Android SDK** являются **32-битными** (appt, adb...), добавьте архитектуру **i386**...:

```
sudo dpkg --add-architecture i386
```

... обновите список пакетов и установите следующие библиотеки:

```
sudo apt-get update
sudo apt-get install libc6:i386 libncurses5:i386 libstdc++6:i386 zlib1g:i386
```

Сборка из исходников
--------------------

[![LazarusIDE](http://wiki.lazarus.freepascal.org/images/9/94/built_with_lazarus_logo.png)](http://www.lazarus-ide.org)

Для корректной сборки используйте **trunk** версию Lazarus IDE:

- http://wiki.lazarus.freepascal.org/Getting_Lazarus#Getting_Lazarus_SVN_development_version

Лицензия
--------

[![GPLv3](http://www.gnu.org/graphics/gplv3-127x51.png)](https://github.com/Helltar/AMPASIDE/blob/master/COPYING)
