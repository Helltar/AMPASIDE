Тесты проводились на данных дистрибутивах, запуск работает без проблем:

- **Ubuntu 16.04.7** LTS (Xenial Xerus)
- **Xubuntu 20.04.4** LTS (Focal Fossa) (**Xfce**, **KDE**)
- **Ubuntu 22.04** LTS (Jammy Jellyfish)


На некоторых системах, например **Fedora 35 Workstation**, вы можете получить ошибку ввида:

```
./ampaside: error while loading shared libraries: libgdk-x11-2.0.so.0: cannot open shared object file: No such file or directory
```

Нужно установить **gtk2**, пример для Fedora:

```
sudo dnf install gtk2
```

Для работы эмулятора должна быть установлена **JRE**:

```
java --version
```

## Для возможности сборки под Android

Должны быть установлены:

**JDK**:

```
javac --version
```

**Apache Ant**:

``` bash
ant --version
```

Некоторые утилиты **Android SDK** являются **32-битными** (appt, adb...), для работы на 64-битной системе установите следующие библиотеки, пример для Ubuntu:

```
sudo apt install libc6:i386 libncurses5:i386 libstdc++6:i386 zlib1g:i386
```
