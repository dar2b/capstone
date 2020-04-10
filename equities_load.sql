use securities; 

show databases; 

CREATE USER 'dar2b'@'local';
GRANT ALL ON securities.* TO 'dar2b'@'local';
FLUSH PRIVILEGES;


-- Schema Design
CREATE TABLE equities_data (
	equities_data_id MEDIUMINT NOT NULL AUTO_INCREMENT,
    date char(200),                         /*TIMESTAMP not used for hours, mintues, and seconds. Using natural PK versus suraget */ 
    vendor_id varchar(32) NOT NULL,
    ticker_id varchar(32) NOT NULL, 
	Open decimal(19,1) NULL,
    High decimal(19,4) NULL, 
    Low decimal(19,4) NULL, 
    Close decimal(19,4) NULL, 
    Volume decimal(19,4) NULL, 
    Adjusted decimal(19,4) NULL, 
    PRIMARY KEY(`equities_data_id`),
    KEY index_data_vendor_id (`vendor_id`),
	KEY index_ticker_id (`ticker_id`)
) ENGINE = InnoDB, AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;


LOAD DATA INFILE "C:/ProgramData/MySQL/MySQL Server 5.6/Uploads/HL_series.csv"
INTO TABLE equities_data
FIELDS terminated by ','
(date, Open, High, Low, Close, Volume, Adjusted)
SET equities_data_id = NULL;

#LINES terminated by '\n';
#ENCLOSED BY ""
#LINES terminated by '\n'
#IGNORE 1 LINES; 

LOAD DATA INFILE "C:/ProgramData/MySQL/MySQL Server 5.6/Uploads/IGOI_series.csv"
INTO TABLE equities_data
FIELDS terminated by ','
ENCLOSED BY ""
LINES terminated by '\n'
ignore 1 lines; 

select * from equities_data;




    
    
	


 
