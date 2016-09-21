Установка
---------

### Linux

Выпускаемые версии на **Qt**, установите [Free Pascal Qt4 Binding](http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html):

``` bash
# Arch Linux
pacman -S qt4pas

# Ubuntu
apt-get install libqt4pas-dev
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

``` bash
# Arch Linux
pacman -S apache-ant

# Ubuntu
apt-get install ant
```

##### Linux x86_64

Некоторые утилиты **Android SDK** являются **32-битными** (appt, adb...), для запуска на:

###### Ubuntu

Добавьте архитектуру **i386**...:

```
dpkg --add-architecture i386
```

... обновите список пакетов и установите следующие библиотеки:

```
apt-get update
apt-get install libc6:i386 libncurses5:i386 libstdc++6:i386 zlib1g:i386
```

###### Arch Linux

- https://wiki.archlinux.org/index.php/Multilib_(Русский)
