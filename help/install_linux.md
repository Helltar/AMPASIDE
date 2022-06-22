Тесты проводились на данных дистрибутивах, запуск работает без проблем:

- Ubuntu 16.04.7 LTS
- Xubuntu 20.04.4 LTS (Xfce, KDE)
- Ubuntu 22.04 LTS


На некоторых системах вы можете получить ошибку вида:

```
error while loading shared libraries: libgdk-x11-2.0.so.0: cannot open shared object file: No such file or directory
```

Нужно установить **gtk2**, пример для Fedora:

```
sudo dnf install gtk2
```

Ubuntu и производные:

```
sudo apt install libgtk2.0-0
```

Для работы эмулятора должна быть установлена **JRE**:

```
java -version
```

Для возможности сборки под Android, установите или переключитесь на JDK **1.8**:

```
javac -version
    javac 1.8.0_332
```

**Apache Ant**:

``` bash
ant --version
```

Некоторые утилиты **Android SDK** являются *32-битными* (appt, adb...), для работы на *64-битной* системе нужны следующие библиотеки, пример установки на Ubuntu:

```
sudo apt install libc6:i386 libncurses5:i386 libstdc++6:i386 zlib1g:i386
```
