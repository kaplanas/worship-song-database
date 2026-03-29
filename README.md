# Worship song database

This project consists of four parts:

- A database of Christian hymns and other worship songs, focused especially on four-part a cappella arrangements used in the Churches of Christ
- [Worship Song Database Manager](https://wsdb.worshipsongfinder.com): a Shiny app for managing the database (auhorized users only; login required)
- [Worship Song Finder](https://wsf.worshipsongfinder.com): a Shiny app for searching the database
- [Observing Congregational Hymnody](https://och.worshipsongfinder.com): a Shiny app in which individual congregations can track the songs they sing during worship, linked to the database (login required; see site's help page for info on how to get a login)

# The database

If you want to download the database yourself and explore, you can find the raw data under [database_files](database_files).

Warning: this is <i>not</i> just a single flat table?  Check out the ERD for a guide to how things are structured.  It's not quite 3NF, but it's close.

- If you're comfortable in (My)SQL, you can get a [dump](database_files/wsdb_dump.sql) of the database.
- If you just want to look at the raw tables with low overhead, you can get a [zip file](database_files/wsdb_csvs.zip) of each raw table in as a csv.  (Not explicitly shown in the [ERD](database_files/wsdb_erd.png) is the fact that each many-to-many relationship&mdash;and there are a <i>lot</i> of them&mdash;gets is own bridge table.)
