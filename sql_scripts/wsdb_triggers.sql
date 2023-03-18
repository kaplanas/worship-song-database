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