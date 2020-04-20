ALTER TABLE `test_group_confs`
    ADD COLUMN `is_archived` TINYINT(1) NOT NULL DEFAULT 0;

ALTER TABLE `student_groups`
    ADD COLUMN `is_archived` TINYINT(1) NOT NULL DEFAULT 0;