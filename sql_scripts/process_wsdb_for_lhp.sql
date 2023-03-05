-- For each hymnologist, create a pretty label with the hymnologist's
-- name.
CREATE OR REPLACE VIEW lhp.hymnologist_labels AS
SELECT HymnologistID, FirstName, LastName,
       CONCAT(CASE WHEN FirstName IS NULL THEN ''
                   ELSE CONCAT(FirstName, ' ')
			  END,
              LastName) AS HymnologistLabel
FROM lhp.hymnologists;

-- For each song, associate the song ID with a unique label for the
-- drop-down menus.  The label should list all attested titles of the
-- song in the five reference hymnals.
CREATE OR REPLACE VIEW lhp.song_labels AS
WITH possible_instances AS
     (SELECT DISTINCT songinstances.SongID, songinstances.SongInstance,
             CONCAT(songbooks.SongbookAbbreviation,
                    CONCAT(' ', songbookentries.EntryNumber)) AS Entry
	  FROM wsdb.songinstances
           JOIN wsdb.songbookentries
           ON songinstances.SongInstanceID = songbookentries.SongInstanceID
           JOIN wsdb.songbooks
           ON songbookentries.SongbookID = songbooks.SongbookID
	  WHERE songbooks.SongbookID IN
            (1, 6, 3, 4, 2)),
     possible_songs AS
	 (SELECT songs.SongID, songs.SongName, songs.SongDisambiguator,
             GROUP_CONCAT(possible_instances.Entry
                          ORDER BY possible_instances.Entry
                          SEPARATOR ', ') AS Entries
      FROM wsdb.songs
           JOIN possible_instances
           ON songs.SongID = possible_instances.SongID
	  GROUP BY songs.SongID, songs.SongName, songs.SongDisambiguator),
     song_titles AS
	 (SELECT SongID, SongName,
             CONCAT('<b>',
                    CONCAT(SongName,
                           CASE WHEN SongDisambiguator IS NULL THEN ''
                                ELSE CONCAT(' (',
                                            CONCAT(SongDisambiguator,
                                                   ')'))
					       END),
                           '</b>') AS DisambiguatedSongName,
			 Entries
	  FROM possible_songs),
	 other_titles AS
     (SELECT DISTINCT possible_instances.SongID,
             possible_instances.SongInstance
	  FROM possible_instances
           JOIN song_titles
           ON possible_instances.SongID = song_titles.SongID
	  WHERE possible_instances.SongInstance <> song_titles.SongName
            AND possible_instances.SongInstance <> song_titles.DisambiguatedSongName
            AND possible_instances.SongInstance <> ''),
	 other_titles_concat AS
     (SELECT SongID,
             GROUP_CONCAT(SongInstance
                          ORDER BY SongInstance
                          SEPARATOR '\n') AS OtherTitles
      FROM other_titles
      GROUP BY SongID)
SELECT song_titles.SongID,
       CONCAT(song_titles.DisambiguatedSongName,
              CONCAT('\n',
                     CONCAT(song_titles.Entries,
			                CASE WHEN other_titles_concat.SongID IS NULL
                                      THEN ''
			                     ELSE CONCAT('\n', other_titles_concat.OtherTitles)
			                END))) AS SongLabel
FROM song_titles
     LEFT JOIN other_titles_concat
     ON song_titles.SongID = other_titles_concat.SongID;