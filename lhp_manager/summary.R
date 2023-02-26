summary.table.info = list(
  sql = "WITH song_titles AS
              (SELECT SongID,
                      CONCAT(SongName,
                             CASE WHEN SongDisambiguator IS NULL THEN ''
                                  ELSE CONCAT(' (',
                                              CONCAT(SongDisambiguator,
                                                     ')'))
                             END) AS Song
               FROM wsdb.songs),
              entry_numbers AS
              (SELECT songinstances.SongID, songbooks.SongbookAbbreviation,
                      GROUP_CONCAT(songbookentries.EntryNumber
                                   ORDER BY songbookentries.EntryNumber
                                   SEPARATOR ', ') AS EntryNumbers
               FROM wsdb.songbookentries
                    JOIN wsdb.songinstances
                    ON songbookentries.SongInstanceID = songinstances.SongInstanceID
                    JOIN wsdb.songbooks
                    ON songbookentries.SongbookID = songbooks.SongbookID
               GROUP BY songinstances.SongID, songbooks.SongbookAbbreviation),
              return_counts AS
              (SELECT SongID,
                      COUNT(DISTINCT hymnologist_returns.HymnologistID) AS Hymnologists
               FROM lhp.hymnologist_returns
               GROUP BY SongID)
         SELECT song_titles.Song, return_counts.Hymnologists,
                sfp.EntryNumbers AS \"Songs of Faith and Praise\",
                pl.EntryNumbers AS \"Praise for the Lord\",
                phss.EntryNumbers AS \"Psalms, Hymns, and Spiritual Songs\",
                hw.EntryNumbers AS \"Hymns for Worship\",
                tph.EntryNumbers AS \"The Paperless Hymnal\"
         FROM return_counts
              JOIN song_titles
              ON return_counts.SongID = song_titles.SongID
              LEFT JOIN entry_numbers sfp
              ON return_counts.SongID = sfp.SongID
                 AND sfp.SongbookAbbreviation = 'SFP'
              LEFT JOIN entry_numbers pl
              ON return_counts.SongID = pl.SongID
                 AND pl.SongbookAbbreviation = 'PL'
              LEFT JOIN entry_numbers phss
              ON return_counts.SongID = phss.SongID
                 AND phss.SongbookAbbreviation = 'PHSS'
              LEFT JOIN entry_numbers hw
              ON return_counts.SongID = hw.SongID
                 AND hw.SongbookAbbreviation = 'HW'
              LEFT JOIN entry_numbers tph
              ON return_counts.SongID = tph.SongID
                 AND tph.SongbookAbbreviation = 'TPH'
         ORDER BY return_counts.Hymnologists DESC, song_titles.Song"
)

summary.page = tabPanel("Summary",
                        DTOutput("lhp.summary"))