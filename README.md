AMPASIDE
========

Advanced MIDletPascal IDE

Среда разработки использующая компилятор языка программирования [MIDletPascal](http://ru.wikipedia.org/wiki/MIDletPascal).

<p align="center"><img src="http://helltar.ho.ua/trash/ampaside_0.9.3v2.png" alt="ampaside-0.9" /></p>

Установка
---------

### Linux

Выпускаемые версии на **Qt**, установите [Free Pascal Qt4 Binding](http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html):

``` bash
# yum, zypper, etc...
sudo apt-get install libqt4pas-dev
```

(при необходимости под **GTK** вы можете собрать из исходников)

Скачайте [последний релиз](https://github.com/Helltar/AMPASIDE/releases/latest), распакуйте наприм. в *~/programs/ampaside/*, дайте права на исполнение:
``` bash
cd ~/programs/ampaside/
chmod +x ampaside tools/linux_x86_64/* # linux_i386
```

Для запуска эмулятора должна быть установлена **JRE**:
```
java -version
```

Сборка из исходников
--------------------

[![LazarusIDE](http://wiki.lazarus.freepascal.org/images/9/94/built_with_lazarus_logo.png)](http://www.lazarus.freepascal.org)

Для корректной сборки используйте **trunk** версию Lazarus IDE:

- http://wiki.lazarus.freepascal.org/Getting_Lazarus#Getting_SVN

Лицензия
--------

[![GPLv3](http://www.gnu.org/graphics/gplv3-127x51.png)](https://github.com/Helltar/AMPASIDE/blob/master/COPYING)
