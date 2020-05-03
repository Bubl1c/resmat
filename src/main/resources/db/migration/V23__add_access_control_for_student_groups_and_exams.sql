create table users_exam_confs
(
    user_id      bigint not null,
    exam_conf_id bigint not null,
    primary key (user_id, exam_conf_id),
    constraint users_exam_confs_users_fk
        foreign key (user_id) references users (id)
            on delete cascade,
    constraint users_exam_confs_exam_confs_fk
        foreign key (exam_conf_id) references exam_confs (id)
            on delete cascade
) default charset = utf8;

create table users_student_groups
(
    user_id      bigint not null,
    student_group_id bigint not null,
    primary key (user_id, student_group_id),
    constraint users_student_groups_users_fk
        foreign key (user_id) references users (id)
            on delete cascade,
    constraint users_student_groups_student_groups_fk
        foreign key (student_group_id) references student_groups (id)
            on delete cascade
) default charset = utf8;

create table users_test_groups
(
    user_id      bigint not null,
    test_group_id bigint not null,
    primary key (user_id, test_group_id),
    constraint users_test_groups_users_fk
        foreign key (user_id) references users (id)
            on delete cascade,
    constraint users_test_groups_test_test_group_confs_fk
        foreign key (test_group_id) references test_group_confs (id)
            on delete cascade
) default charset = utf8;
