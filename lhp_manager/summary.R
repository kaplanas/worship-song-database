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
              (SELECT songinstances.SongID, songbooks.SongbookID,
                      GROUP_CONCAT(songbookentries.EntryNumber
                                   ORDER BY songbookentries.EntryNumber
                                   SEPARATOR ', ') AS EntryNumbers
               FROM wsdb.songbookentries
                    JOIN wsdb.songinstances
                    ON songbookentries.SongInstanceID = songinstances.SongInstanceID
                    JOIN wsdb.songbooks
                    ON songbookentries.SongbookID = songbooks.SongbookID
               GROUP BY songinstances.SongID, songbooks.SongbookID),
              return_counts AS
              (SELECT SongID,
                      COUNT(DISTINCT hymnologist_returns.HymnologistID) AS Hymnologists
               FROM lhp.hymnologist_returns
               WHERE Processed
               GROUP BY SongID)
         SELECT song_titles.Song, return_counts.Hymnologists,
                sfp.EntryNumbers AS \"Songs of Faith and Praise\",
                pl.EntryNumbers AS \"Praise for the Lord\",
                phss.EntryNumbers AS \"Psalms, Hymns, and Spiritual Songs\",
                hw.EntryNumbers AS \"Hymns for Worship\",
                tph.EntryNumbers AS \"The Paperless Hymnal\",
                CASE WHEN christmas.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Christmas,
                CASE WHEN nation.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Nation
         FROM return_counts
              JOIN song_titles
              ON return_counts.SongID = song_titles.SongID
              LEFT JOIN entry_numbers sfp
              ON return_counts.SongID = sfp.SongID
                 AND sfp.SongbookID = 1
              LEFT JOIN entry_numbers pl
              ON return_counts.SongID = pl.SongID
                 AND pl.SongbookID = 6
              LEFT JOIN entry_numbers phss
              ON return_counts.SongID = phss.SongID
                 AND phss.SongbookID = 21
              LEFT JOIN entry_numbers hw
              ON return_counts.SongID = hw.SongID
                 AND hw.SongbookID = 22
              LEFT JOIN entry_numbers tph
              ON return_counts.SongID = tph.SongID
                 AND tph.SongbookID = 12
              LEFT JOIN wsdb.songs_topics christmas
              ON return_counts.SongID = christmas.SongID
                 AND christmas.TopicID = 1
              LEFT JOIN wsdb.songs_topics nation
              ON return_counts.SongID = nation.SongID
                 AND nation.TopicID = 2
         ORDER BY return_counts.Hymnologists DESC, song_titles.Song"
)

summary.table = tabPanel(
  "Table of songs",
  DTOutput("lhp.summary")
)

summary.histogram = tabPanel(
  "Distribution of votes",
  plotOutput("lhp.histogram")
)

summary.page = tabPanel("Summary",
                        navlistPanel(
                          summary.table,
                          summary.histogram,
                          well = F,
                          widths = c(2, 10)
                        ))