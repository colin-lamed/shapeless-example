CREATE PROCEDURE MY_PROCEDURE(IN one VARCHAR(1000), IN two VARCHAR(1000), OUT res1 VARCHAR(2000), OUT res2 BOOLEAN)
BEGIN ATOMIC
    set res1 = one + two;
    set res2 = one = two;
END;