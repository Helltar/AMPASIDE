Тесты проводились на данных дистрибутивах, запуск работает без проблем:

- Ubuntu 16.04.7 LTS
- Xubuntu 20.04.4 LTS (Xfce, KDE)
- Ubuntu 22.04 LTS

На некоторых системах вы можете получить ошибку вида:

```
error while loading shared libraries: libgdk-x11-2.0.so.0: cannot open shared object file: No such file or directory
```

Нужно установить **qt5pas**, пример для **Fedora**:

```
sudo dnf install qt5pas
```

**Arch Linux**:

```
sudo pacman -S qt5pas
```

**Ubuntu** и производные:

```
sudo apt install libqt5pas-dev
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
