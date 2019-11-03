#!/bin/sh

for FILE in *.py; do
    ENCODING=`file --brief --mime-encoding "$FILE"`
    if [ "$ENCODING" == 'iso-8859-1' ]; then
	echo "Converting $FILE to utf8"
        cp "$FILE" "$FILE.iso-8859-1"
	iconv -f 'ISO-8859-1' -t 'UTF-8' "$FILE" > "$FILE.utf8"
	cp "$FILE.utf8" "$FILE"
    fi
done
