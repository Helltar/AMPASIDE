Для запуска эмулятора должна быть установлена **JRE**:

```
java --version
```

### Для возможности сборки под Android

Должен быть установлен **JDK**:

```
javac --version
```

**Apache Ant**:

``` bash
ant --version
```

#### Linux x86_64

Некоторые утилиты **Android SDK** являются **32-битными** (appt, adb...), для запуска установите следующие библиотеки:

``` bash
# Ubuntu
sudo apt install libc6:i386 libncurses5:i386 libstdc++6:i386 zlib1g:i386
```
