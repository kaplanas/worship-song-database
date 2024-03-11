#### Label tables ####

gender.labels.sql = "SELECT NULL AS GenderID, '' AS GenderLabel
                     FROM dual
                     UNION ALL
                     SELECT GenderID, GenderLabel
                     FROM wsdb.gender_labels"

artist.labels.sql = "SELECT ArtistID, ArtistLabel
                     FROM wsdb.artist_labels"

copyright.administrator.labels.sql = "SELECT NULL AS CopyrightAdministratorID,
                                             '' AS CopyrightAdministratorLabel
                                      FROM dual
                                      UNION ALL
                                      SELECT CopyrightAdministratorID,
                                             CopyrightAdministratorLabel
                                      FROM wsdb.copyrightadministrator_labels"

copyright.holder.labels.sql = "SELECT CopyrightHolderID, CopyrightHolderLabel
                               FROM wsdb.copyrightholder_labels"

language.labels.sql = "SELECT LanguageID, LanguageLabel
                       FROM wsdb.language_labels"

book.of.the.bible.labels.sql = "SELECT BookID, BookLabel
                                FROM wsdb.bookofthebible_labels"

scripture.reference.labels.sql = "SELECT ScriptureReferenceID,
                                         ScriptureReferenceLabel
                                  FROM wsdb.scripturereference_labels"

meter.labels.sql = "SELECT MeterID, MeterLabel
                    FROM wsdb.meter_labels"

pitch.labels.sql = "SELECT PitchID, PitchLabel
                    FROM wsdb.pitch_labels"

accidental.labels.sql = "SELECT AccidentalID, AccidentalLabel
                         FROM wsdb.accidental_labels"

mode.labels.sql = "SELECT ModeID, ModeLabel
                   FROM wsdb.mode_labels"

key.signature.labels.sql = "SELECT KeySignatureID, KeySignatureLabel
                            FROM wsdb.keysignature_labels"

time.signature.labels.sql = "SELECT TimeSignatureID, TimeSignatureLabel
                             FROM wsdb.timesignature_labels"

lyrics.labels.sql = "SELECT LyricsID, LyricsLabel
                     FROM wsdb.lyrics_labels"

tune.labels.sql = "SELECT TuneID, TuneLabel
                   FROM wsdb.tune_labels"

arrangement.type.labels.sql = "SELECT ArrangementTypeID, ArrangementTypeLabel
                               FROM wsdb.arrangementtype_labels"

arrangement.labels.sql = "SELECT ArrangementID, ArrangementLabel
                          FROM wsdb.arrangement_labels"

song.instance.labels.sql = "SELECT SongInstanceID, SongInstanceLabel
                            FROM wsdb.songinstance_labels"

topic.labels.sql = "SELECT TopicID, TopicLabel
                    FROM wsdb.topic_labels"

song.type.labels.sql = "SELECT SongTypeID, SongTypeLabel
                        FROM wsdb.songtype_labels"

song.tempo.labels.sql = "SELECT SongTempoID, SongTempoLabel
                         FROM wsdb.songtempo_labels"

song.labels.sql = "SELECT SongID, SongLabel
                   FROM wsdb.song_labels"

psalm.number.labels.sql = "WITH RECURSIVE psalmnumbers (PsalmNumber) AS
                             (SELECT 1 AS PsalmNumber
                              UNION
                              SELECT PsalmNumber + 1
                              FROM psalmnumbers
                              WHERE PsalmNumber < 150)
                           SELECT PsalmNumber, PsalmNumber AS PsalmNumberLabel
                           FROM psalmnumbers"

metrical.psalm.labels.sql = "SELECT MetricalPsalmID, MetricalPsalmLabel
                             FROM wsdb.metricalpsalm_labels"

psalm.song.labels.sql = "SELECT SongID, SongLabel
                         FROM wsdb.song_labels
                         WHERE SongID IN (SELECT SongID
                                          FROM wsdb.psalmsongs)"

tune.song.labels.sql = "SELECT TuneID, TuneLabel
                        FROM wsdb.tune_song_labels
                        ORDER BY TuneLabel"

#### Combined info ####

label.table.sql = list(
  gender.labels = gender.labels.sql,
  artist.labels = artist.labels.sql,
  copyright.administrator.labels = copyright.administrator.labels.sql,
  copyright.holder.labels = copyright.holder.labels.sql,
  language.labels = language.labels.sql,
  book.of.the.bible.labels = book.of.the.bible.labels.sql,
  scripture.reference.labels = scripture.reference.labels.sql,
  meter.labels = meter.labels.sql,
  pitch.labels = pitch.labels.sql,
  accidental.labels = accidental.labels.sql,
  mode.labels = mode.labels.sql,
  key.signature.labels = key.signature.labels.sql,
  time.signature.labels = time.signature.labels.sql,
  lyrics.labels = lyrics.labels.sql,
  tune.labels = tune.labels.sql,
  arrangement.labels = arrangement.labels.sql,
  song.instance.labels = song.instance.labels.sql,
  topic.labels = topic.labels.sql,
  song.type.labels = song.type.labels.sql,
  song.tempo.labels = song.tempo.labels.sql,
  song.labels = song.labels.sql,
  arrangement.type.labels = arrangement.type.labels.sql,
  psalm.number.labels = psalm.number.labels.sql,
  metrical.psalm.labels = metrical.psalm.labels.sql,
  psalm.song.labels = psalm.song.labels.sql,
  tune.song.labels = tune.song.labels.sql
)

#### Useful functions ####

# Populate the table
populate.label.table = function(label.table, db.con,
                                reactive.reference.tables) {
  if(!is.null(db.con)) {
    label.sql = label.table.sql[[label.table]]
    tryCatch(
      {
        return(dbGetQuery(db.con, label.sql))
      },
      error = function(err) {
        print(err)
        return(NULL)
      }
    )
  } else {
    return(NULL)
  }
}
