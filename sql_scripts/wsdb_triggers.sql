-- A function that takes a meter string (e.g., "8.6.8.6"), and returns
-- a string of letters that we can use to sort meters in a useful way.

DROP FUNCTION IF EXISTS wsdb.compute_meter_sort_string;

DELIMITER //

CREATE FUNCTION wsdb.compute_meter_sort_string(Meter VARCHAR(50))
RETURNS VARCHAR(50) DETERMINISTIC
BEGIN
	
    DECLARE working_string VARCHAR(50);
    DECLARE next_position INTEGER;
    DECLARE number_of_syllables INTEGER;
	DECLARE sort_string VARCHAR(50);
    
    SET working_string = Meter;
    SET sort_string = '';
    
    IF REGEXP_LIKE(working_string, '^[0-9]+([.][0-9]+)*$') THEN
		WHILE LENGTH(working_string) > 0 DO
			SET next_position = POSITION('.' IN working_string);
			IF next_position = 0 THEN
				SET next_position = LENGTH(working_string) + 1;
			END IF;
			SET number_of_syllables = CAST(SUBSTRING(working_string, 1,
                                                     next_position - 1)
										   AS UNSIGNED);
			SET sort_string = CONCAT(sort_string, CONV(9 + number_of_syllables, 10, 36));
			IF next_position < LENGTH(working_string) THEN
				SET working_string = SUBSTRING(working_string,
                                               POSITION('.' IN working_string) + 1);
			ELSE
				SET working_string = '';
			END IF;
		END WHILE;
    END IF;
    
    RETURN sort_string;
    
END; //

DELIMITER ;

-- Every time we insert a new row into the table of meters, automatically
-- compute the sort string.

DELIMITER //

CREATE TRIGGER wsdb.after_meters_insert BEFORE INSERT ON wsdb.meters
FOR EACH ROW
BEGIN
    SET NEW.SortString = compute_meter_sort_string(NEW.Meter);
END; //

CREATE TRIGGER wsdb.after_meters_update BEFORE UPDATE ON wsdb.meters
FOR EACH ROW
BEGIN
    SET NEW.SortString = compute_meter_sort_string(NEW.Meter);
END; //

DELIMITER ;

-- A function that takes a song instance ID and returns a pretty string
-- the concatenates all the scripture references associated with the
-- song instance or metrical psalm.

DROP FUNCTION IF EXISTS wsdb.compute_pretty_scripture_lists;

DELIMITER //

CREATE FUNCTION wsdb.compute_pretty_scripture_lists(the_id INTEGER,
                                                    id_type INTEGER)
RETURNS VARCHAR(1000) DETERMINISTIC
BEGIN
	
    DECLARE book_id INTEGER;
    DECLARE pretty_string VARCHAR(1000);
    DECLARE current_continuous_verses VARCHAR(100);
    DECLARE current_book VARCHAR(100);
    DECLARE last_book VARCHAR(100);
    DECLARE penult_book VARCHAR(100);
    DECLARE current_chapter INTEGER;
    DECLARE last_chapter INTEGER;
    DECLARE current_verse INTEGER;
    DECLARE last_verse INTEGER;
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT booksofthebible.BookID, BookAbbreviation,
               Chapter, Verse
        FROM (SELECT SongInstanceID AS TheID, LyricsID,
                     0 AS PsalmNumber
              FROM wsdb.songinstances_lyrics
              WHERE SongInstanceID = the_id
                    AND id_type = 1
              UNION ALL
              SELECT metricalpsalms.MetricalPsalmID AS TheID, LyricsID,
                     PsalmNumber
              FROM wsdb.metricalpsalms
                   JOIN wsdb.metricalpsalms_lyrics
                   ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
              WHERE metricalpsalms.MetricalPsalmID = the_id
                    AND id_type = 2
			  UNION ALL
              SELECT psalmsongs.SongID AS TheID, LyricsID, PsalmNumber
              FROM wsdb.psalmsongs
                   JOIN wsdb.songinstances
                   ON psalmsongs.SongID = songinstances.SongID
                   JOIN wsdb.songinstances_lyrics
                   ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
			  WHERE PsalmSongID = the_id
                    AND id_type = 3) id_lyrics
             JOIN wsdb.lyrics_scripturereferences
             ON id_lyrics.LyricsID = lyrics_scripturereferences.LyricsID
			 JOIN wsdb.scripturereferences
             ON lyrics_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
             JOIN wsdb.booksofthebible
             ON scripturereferences.BookID = booksofthebible.BookID
		WHERE id_type = 1
              OR (booksofthebible.BookID = 19
                  AND Chapter = PsalmNumber)
        ORDER BY booksofthebible.BookID,
				 scripturereferences.Chapter,
                 scripturereferences.Verse;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    SET book_id = 0;
    SET pretty_string = '';
    SET current_continuous_verses = '';
    SET current_book = '';
    SET last_book = '';
    SET current_chapter = 0;
    SET current_verse = 0;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
		
        SET penult_book = last_book;
		SET last_book = current_book;
        SET last_chapter = current_chapter;
        SET last_verse = current_verse;
        
        FETCH curs INTO book_id, current_book, current_chapter, current_verse;
        
        IF current_book = last_book
            AND current_chapter = last_chapter
		    AND current_verse = last_verse + 1 THEN
            
            IF RIGHT(current_continuous_verses, 1) <> '-' THEN
                SET current_continuous_verses = CONCAT(current_continuous_verses, '-');
			END IF;
            
		ELSE
            
            IF RIGHT(current_continuous_verses, 1) = '-' THEN
				SET current_continuous_verses = CONCAT(current_continuous_verses, last_verse);
			END IF;
			
			IF pretty_string <> '' THEN
                IF last_book = penult_book THEN
                    SET pretty_string = CONCAT(pretty_string, ', ');
				ELSE
                    SET pretty_string = CONCAT(pretty_string, '; ');
				END IF;
			END IF;
            
            SET pretty_string = CONCAT(pretty_string, current_continuous_verses);
            
            IF loop_done THEN
                LEAVE read_loop;
			END IF;
            
            IF current_book = last_book THEN
                IF current_chapter = last_chapter THEN
                    SET current_continuous_verses = current_verse;
				ELSE
                    SET current_continuous_verses = CONCAT(current_chapter,
                                                           ':', current_verse);
				END IF;
			ELSE
                SET current_continuous_verses = CONCAT(current_book, ' ',
                                                       current_chapter, ':',
                                                       current_verse);
			END IF;
            
	    END IF;
        
	END LOOP;
    
    CLOSE curs;
    
    RETURN pretty_string;
    
END; //

DELIMITER ;

-- Every time we change a row in the tables that do the many-to-many
-- mapping between lyrics and song instances or metrical psalms, update
-- the appropriate rows in the table that lists the pretty scripture list
-- for each song instances.

DELIMITER //

CREATE TRIGGER wsdb.after_lyrics_scriptures_insert AFTER INSERT ON wsdb.lyrics_scripturereferences
FOR EACH ROW
BEGIN
    
    DECLARE the_id INTEGER;
    DECLARE id_type INTEGER;
    DECLARE pretty_string VARCHAR(1000);
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT SongInstanceID AS TheID, 1 AS IDType
        FROM wsdb.songinstances_lyrics
        WHERE LyricsID = NEW.LyricsID
		UNION ALL
        SELECT DISTINCT MetricalPsalmID AS TheID, 2 AS IDType
        FROM wsdb.metricalpsalms_lyrics
        WHERE LyricsID = NEW.LyricsID
        UNION ALL
        SELECT DISTINCT PsalmSongID AS TheID, 3 AS IDType
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
             JOIN wsdb.songinstances_lyrics
             ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
		WHERE LyricsID = NEW.LyricsID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
        
        FETCH curs INTO the_id, id_type;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(the_id, id_type);
            IF id_type = 1 THEN
				INSERT INTO wsdb.prettyscripturelists
				(SongInstanceID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
			ELSEIF id_type = 2 THEN
				INSERT INTO wsdb.metricalpsalms_prettyscripturelists
				(MetricalPsalmID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            ELSEIF id_type = 3 THEN
				INSERT INTO wsdb.psalmsongs_prettyscripturelists
				(PsalmSongID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            END IF;
		END IF;
        
    END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_lyrics_scriptures_update AFTER UPDATE ON wsdb.lyrics_scripturereferences
FOR EACH ROW
BEGIN
    
    DECLARE the_id INTEGER;
    DECLARE id_type INTEGER;
    DECLARE pretty_string VARCHAR(1000);
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT SongInstanceID AS TheID, 1 AS IDType
        FROM wsdb.songinstances_lyrics
        WHERE LyricsID = NEW.LyricsID
        UNION ALL
        SELECT DISTINCT MetricalPsalmID AS  TheID, 2 AS IDType
        FROM wsdb.metricalpsalms_lyrics
        WHERE LyricsID = NEW.LyricsID
        UNION ALL
        SELECT DISTINCT PsalmSongID AS TheID, 3 AS IDType
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
             JOIN wsdb.songinstances_lyrics
             ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
		WHERE LyricsID = NEW.LyricsID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
        
        FETCH curs INTO the_id, id_type;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(the_id, id_type);
            IF id_type = 1 THEN
				INSERT INTO wsdb.prettyscripturelists
				(SongInstanceID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
			ELSEIF id_type = 2 THEN
				INSERT INTO wsdb.metricalpsalms_prettyscripturelists
				(MetricalPsalmID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            ELSEIF id_type = 3 THEN
				INSERT INTO wsdb.psalmsongs_prettyscripturelists
				(PsalmSongID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            END IF;
		END IF;
        
    END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_lyrics_scriptures_delete AFTER DELETE ON wsdb.lyrics_scripturereferences
FOR EACH ROW
BEGIN
    
    DECLARE the_id INTEGER;
    DECLARE id_type INTEGER;
    DECLARE pretty_string VARCHAR(1000);
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT SongInstanceID AS TheID, 1 AS IDType
        FROM wsdb.songinstances_lyrics
        WHERE LyricsID = OLD.LyricsID
        UNION ALL
        SELECT DISTINCT MetricalPsalmID AS TheID, 2 AS IDType
        FROM wsdb.metricalpsalms_lyrics
        WHERE LyricsID = OLD.LyricsID
        UNION ALL
        SELECT DISTINCT PsalmSongID AS TheID, 3 AS IDType
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
             JOIN wsdb.songinstances_lyrics
             ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
		WHERE LyricsID = OLD.LyricsID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
        
        FETCH curs INTO the_id, id_type;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(the_id, id_type);
            IF id_type = 1 THEN
				INSERT INTO wsdb.prettyscripturelists
				(SongInstanceID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
			ELSEIF id_type = 2 THEN
				INSERT INTO wsdb.metricalpsalms_prettyscripturelists
				(MetricalPsalmID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            ELSEIF id_type = 3 THEN
				INSERT INTO wsdb.psalmsongs_prettyscripturelists
				(PsalmSongID, PrettyScriptureList)
				VALUES
				(the_id, pretty_string)
				ON DUPLICATE KEY UPDATE
				PrettyScriptureList = pretty_string;
            END IF;
		END IF;
        
    END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_songinstance_lyrics_insert AFTER INSERT ON wsdb.songinstances_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    DECLARE psalmsong_id INTEGER;
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT PsalmSongID
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
	    WHERE songinstances.SongInstanceID = NEW.SongInstanceID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    SET pretty_string = wsdb.compute_pretty_scripture_lists(NEW.SongInstanceID, 1);
    INSERT INTO wsdb.prettyscripturelists
    (SongInstanceID, PrettyScriptureList)
    VALUES
    (NEW.SongInstanceID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
		
		FETCH curs INTO psalmsong_id;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(psalmsong_id, 3);
			INSERT INTO wsdb.psalmsongs_prettyscripturelists
			(PsalmSongID, PrettyScriptureList)
			VALUES
			(psalmsong_id, pretty_string)
			ON DUPLICATE KEY UPDATE
			PrettyScriptureList = pretty_string;
		END IF;
        
	END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_songinstance_lyrics_update AFTER UPDATE ON wsdb.songinstances_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    DECLARE psalmsong_id INTEGER;
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT PsalmSongID
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
	    WHERE songinstances.SongInstanceID = NEW.SongInstanceID
              OR songinstances.SongInstanceID = OLD.SongInstanceID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    SET pretty_string = wsdb.compute_pretty_scripture_lists(NEW.SongInstanceID, 1);
    INSERT INTO wsdb.prettyscripturelists
    (SongInstanceID, PrettyScriptureList)
    VALUES
    (NEW.SongInstanceID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    SET pretty_string = wsdb.compute_pretty_scripture_lists(OLD.SongInstanceID, 1);
    INSERT INTO wsdb.prettyscripturelists
    (SongInstanceID, PrettyScriptureList)
    VALUES
    (OLD.SongInstanceID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
		
		FETCH curs INTO psalmsong_id;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(psalmsong_id, 3);
			INSERT INTO wsdb.psalmsongs_prettyscripturelists
			(PsalmSongID, PrettyScriptureList)
			VALUES
			(psalmsong_id, pretty_string)
			ON DUPLICATE KEY UPDATE
			PrettyScriptureList = pretty_string;
		END IF;
        
	END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_songinstance_lyrics_delete AFTER DELETE ON wsdb.songinstances_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    DECLARE psalmsong_id INTEGER;
    DECLARE loop_done INTEGER;
    DECLARE curs CURSOR FOR
        SELECT DISTINCT PsalmSongID
        FROM wsdb.psalmsongs
             JOIN wsdb.songinstances
             ON psalmsongs.SongID = songinstances.SongID
	    WHERE songinstances.SongInstanceID = OLD.SongInstanceID;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET loop_done = true;
    
    SET pretty_string = wsdb.compute_pretty_scripture_lists(OLD.SongInstanceID, 1);
    INSERT INTO wsdb.prettyscripturelists
    (SongInstanceID, PrettyScriptureList)
    VALUES
    (OLD.SongInstanceID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
    OPEN curs;
    
    SET loop_done = false;
    read_loop: LOOP
		
		FETCH curs INTO psalmsong_id;
        IF loop_done THEN
            LEAVE read_loop;
		ELSE
            SET pretty_string = wsdb.compute_pretty_scripture_lists(psalmsong_id, 3);
			INSERT INTO wsdb.psalmsongs_prettyscripturelists
			(PsalmSongID, PrettyScriptureList)
			VALUES
			(psalmsong_id, pretty_string)
			ON DUPLICATE KEY UPDATE
			PrettyScriptureList = pretty_string;
		END IF;
        
	END LOOP;
    
    CLOSE curs;
    
END; //

CREATE TRIGGER wsdb.after_metricalpsalm_lyrics_insert AFTER INSERT ON wsdb.metricalpsalms_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    SET pretty_string = wsdb.compute_pretty_scripture_lists(NEW.MetricalPsalmID, 2);
    INSERT INTO wsdb.metricalpsalms_prettyscripturelists
    (MetricalPsalmID, PrettyScriptureList)
    VALUES
    (NEW.MetricalPsalmID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
END; //

CREATE TRIGGER wsdb.after_metricalpsalm_lyrics_update AFTER UPDATE ON wsdb.metricalpsalms_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    SET pretty_string = wsdb.compute_pretty_scripture_lists(NEW.MetricalPsalmID, 2);
    INSERT INTO wsdb.metricalpsalms_prettyscripturelists
    (MetricalPsalmID, PrettyScriptureList)
    VALUES
    (NEW.MetricalPsalmID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    SET pretty_string = wsdb.compute_pretty_scripture_lists(OLD.MetricalPsalmID, 2);
    INSERT INTO wsdb.metricalpsalms_prettyscripturelists
    (MetricalPsalmID, PrettyScriptureList)
    VALUES
    (OLD.MetricalPsalmID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
END; //

CREATE TRIGGER wsdb.after_metricalpsalm_lyrics_delete AFTER DELETE ON wsdb.metricalpsalms_lyrics
FOR EACH ROW
BEGIN
    
    DECLARE pretty_string VARCHAR(1000);
    SET pretty_string = wsdb.compute_pretty_scripture_lists(OLD.MetricalPsalmID, 2);
    INSERT INTO wsdb.metricalpsalms_prettyscripturelists
    (MetricalPsalmID, PrettyScriptureList)
    VALUES
    (OLD.MetricalPsalmID, pretty_string)
    ON DUPLICATE KEY UPDATE
    PrettyScriptureList = pretty_string;
    
END; //

DELIMITER ;
