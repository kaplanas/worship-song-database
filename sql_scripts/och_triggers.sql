-- Every time we insert a new row into the table of congregations,
-- automatically compute the folder name.  (Don't change anything
-- when updating; we want folder names to stay constant - there's no
-- point in moving stuff around.)

DELIMITER //

CREATE TRIGGER och.after_congregations_insert BEFORE INSERT ON och.congregations
FOR EACH ROW
BEGIN
    DECLARE next_id INTEGER;
    DECLARE cleaned_name VARCHAR(255);
    SELECT IFNULL(AUTO_INCREMENT, 0)
    INTO next_id
    FROM information_schema.tables
	WHERE TABLE_SCHEMA = 'och'
	      AND TABLE_NAME = 'congregations';
	SET cleaned_name = LOWER(NEW.CongregationName);
    SET cleaned_name = REGEXP_REPLACE(cleaned_name, '[^A-Za-z ]', '');
    SET cleaned_name = REGEXP_REPLACE(cleaned_name, ' ', '_');
    SET NEW.FolderName = CONCAT(cleaned_name, '_', next_id);
END; //

DELIMITER ;
