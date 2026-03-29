cd /home/ubuntu/worship-song-database
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TopicID, TopicName FROM wsdb.topics" > database_files/topics.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongID, SongName, SongDisambiguator FROM wsdb.songs" > database_files/songs.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongID, TopicID FROM wsdb.songs_topics" > database_files/songs_topics.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LanguageID, LanguageName FROM wsdb.languages" > database_files/languages.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT BookID, BookName, BookAbbreviation FROM wsdb.booksofthebible" > database_files/booksofthebible.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ScriptureReferenceID, BookID, Chapter, Verse FROM wsdb.scripturereferences" > database_files/scripturereferences.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT MeterID, Meter, Multiplier FROM wsdb.meters" > database_files/meters.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT GenderID, GenderName FROM wsdb.genders" > database_files/genders.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArtistID, LastName, FirstName, Restoration, GenderID FROM wsdb.artists" > database_files/artists.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT CopyrightAdministratorID, CopyrightAdministratorName, AddressLine1, AddressLine2, City, State, ZIPCode, EmailAddress, PhoneNumber FROM wsdb.copyrightadministrators" > database_files/copyrightadministrators.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT CopyrightHolderID, CopyrightHolderName, CopyrightAdministratorID FROM wsdb.copyrightholders" > database_files/copyrightholders.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, FirstLine, RefrainFirstLine, CopyrightYear, LanguageID FROM wsdb.lyrics" > database_files/lyrics.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, ScriptureReferenceID FROM wsdb.lyrics_scripturereferences" > database_files/lyrics_scripturereferences.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, MeterID FROM wsdb.lyrics_meters" > database_files/lyrics_meters.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, ArtistID FROM wsdb.lyrics_artists" > database_files/lyrics_artists.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, TranslatedFromID FROM wsdb.lyrics_translations" > database_files/lyrics_translations.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT LyricsID, CopyrightHolderID FROM wsdb.lyrics_copyrightholders" > database_files/lyrics_copyrightholders.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TuneID, TuneName, RealTuneName, CopyrightYear FROM wsdb.tunes" > database_files/tunes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TuneID, MeterID FROM wsdb.tunes_meters" > database_files/tunes_meters.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TuneID, ArtistID FROM wsdb.tunes_artists" > database_files/tunes_artists.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TuneID, CopyrightHolderID FROM wsdb.tunes_copyrightholders" > database_files/tunes_copyrightholders.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArrangementTypeID, ArrangementType FROM wsdb.arrangementtypes" > database_files/arrangementtypes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArrangementID, ArrangementName, CopyrightYear FROM wsdb.arrangements" > database_files/arrangements.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArrangementID, ArtistID FROM wsdb.arrangements_artists" > database_files/arrangements_artists.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArrangementID, CopyrightHolderID FROM wsdb.arrangements_copyrightholders" > database_files/arrangements_copyrightholders.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ArrangementID, ArrangementTypeID FROM wsdb.arrangements_arrangementtypes" > database_files/arrangements_arrangementtypes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT PitchID, PitchName FROM wsdb.pitches" > database_files/pitches.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT AccidentalID, AccidentalSymbol FROM wsdb.accidentals" > database_files/accidentals.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT ModeID, ModeName FROM wsdb.modes" > database_files/modes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT KeySignatureID, PitchID, AccidentalID, ModeID FROM wsdb.keysignatures" > database_files/keysignatures.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT TimeSignatureID, TimeSignatureBeat, TimeSignatureMeasure FROM wsdb.timesignatures" > database_files/timesignatures.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongInstanceID, SongInstance, SongID, ArrangementID FROM wsdb.songinstances" > database_files/songinstances.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongInstanceID, LyricsID FROM wsdb.songinstances_lyrics" > database_files/songinstances_lyrics.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongInstanceID, TuneID FROM wsdb.songinstances_tunes" > database_files/songinstances_tunes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongInstanceID, KeySignatureID FROM wsdb.songinstances_keysignatures" > database_files/songinstances_keysignatures.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongInstanceID, TimeSignatureID FROM wsdb.songinstances_timesignatures" > database_files/songinstances_timesignatures.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongbookID, SongbookName, SongbookAbbreviation FROM wsdb.songbooks" > database_files/songbooks.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongbookVolumeID, SongbookVolume FROM wsdb.songbookvolumes" > database_files/songbookvolumes.csv
/home/ubuntu/.local/bin/mycli -h $WSDB_HOST -p $WSDB_PWD -u root -D wsdb --csv -e "SELECT SongbookEntryID, EntryNumber, SongbookID, SongbookVolumeID, SongInstanceID FROM wsdb.songbookentries" > database_files/songbookentries.csv
zip database_files/wsdb_csvs.zip database_files/*.csv
