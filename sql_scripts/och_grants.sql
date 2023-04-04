CREATE ROLE 'observing_congregational_hymnody';

GRANT SELECT, INSERT, UPDATE, DELETE ON och.congregations TO 'observing_congregational_hymnody';
GRANT SELECT, INSERT, UPDATE, DELETE ON och.worshiphistory TO 'observing_congregational_hymnody';

GRANT SELECT ON och.* TO 'observing_congregational_hymnody';
GRANT SELECT ON wsdb.song_labels TO 'observing_congregational_hymnody';
GRANT SELECT ON wsdb.songs TO 'observing_congregational_hymnody';
