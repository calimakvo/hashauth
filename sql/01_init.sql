CREATE TABLE public.admin_usr (
	admin_id bigserial NOT NULL,
	login_id varchar(256) NOT NULL,
	password varchar(512) NOT NULL,
    valid_flag boolean,
	create_time timestamptz NOT NULL,
	update_time timestamptz NOT NULL DEFAULT now(),
	version integer NOT NULL,
	CONSTRAINT admin_usr_pk PRIMARY KEY (admin_id)
);
