#!/bin/bash
hscpp=c2hs
unset CPATH
${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" Graphics/UI/Clutter/Types.chs
${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" -i Graphics/UI/Clutter/Types Graphics/UI/Clutter/Color.chs

${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" -i Graphics/UI/Clutter/Types Graphics/UI/Clutter/Actor.chs

${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" -i Graphics/UI/Clutter/Types Graphics/UI/Clutter/Stage.chs

${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" -i Graphics/UI/Clutter/Types Graphics/UI/Clutter/General.chs

${hscpp} --cppopts="-I/usr/include/clutter-1.0/" --cppopts="-I/usr/include/glib-2.0" --cppopts="-I/usr/include/glib-2.0/glib" --cppopts="-I/usr/lib/glib-2.0/include" --cppopts="-I/usr/include/pango-1.0/" --cppopts="-I/usr/include/cairo" -i Graphics/UI/Clutter/Types Graphics/UI/Clutter/Rectangle.chs



runghc Setup.hs configure
runghc Setup.hs build

cp dist/build/arst/arst .

