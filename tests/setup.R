if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
saeb_cat <- get_catalog( "saeb" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( saeb_cat ) ) / ceiling( nrow( saeb_cat ) / 3 ) )
saeb_cat <- saeb_cat[ record_categories == this_sample_break , ]
saeb_cat <- lodown( "saeb" , saeb_cat )
if( any( saeb_cat$year == 2015 ) ){
column_names <-
	names( 
		read.csv( 
			file.path( getwd() , "2015" , "escolas.csv" ) , 
			nrow = 1 )[ FALSE , , ] 
	)

column_names <- gsub( "\\." , "_" , tolower( column_names ) )

column_types <-
	ifelse( 
		SAScii::parse.SAScii(
			file.path( getwd() , "2015" , "import.sas" ) 
		) , 
		'n' , 'c' 
	)

columns_to_import <-
	c( "entity_type_code" , "provider_gender_code" , "provider_enumeration_date" ,
	"is_sole_proprietor" , "provider_business_practice_location_address_state_name" )

stopifnot( all( columns_to_import %in% column_names ) )

saeb_df <- 
	data.frame( 
		readr::read_csv( 
			file.path( getwd() , 
				"escolas.csv" ) , 
			col_names = columns_to_import , 
			col_types = 
				paste0( 
					ifelse( column_names %in% columns_to_import , column_types , '_' ) , 
					collapse = "" 
				) ,
			skip = 1
		) 
	)

dbSendQuery( db , "ALTER TABLE ADD COLUMN individual INTEGER" )

dbSendQuery( db , 
	"UPDATE 
	SET individual = 
		CASE WHEN entity_type_code = 1 THEN 1 ELSE 0 END" 
)

dbSendQuery( db , "ALTER TABLE ADD COLUMN provider_enumeration_year INTEGER" )

dbSendQuery( db , 
	"UPDATE 
	SET provider_enumeration_year = 
		CAST( SUBSTRING( provider_enumeration_date , 7 , 10 ) AS INTEGER )" 
)
nrow( saeb_df )

table( saeb_df[ , "provider_gender_code" ] , useNA = "always" )
mean( saeb_df[ , "provider_enumeration_year" ] )

tapply(
	saeb_df[ , "provider_enumeration_year" ] ,
	saeb_df[ , "provider_gender_code" ] ,
	mean 
)
prop.table( table( saeb_df[ , "is_sole_proprietor" ] ) )

prop.table(
	table( saeb_df[ , c( "is_sole_proprietor" , "provider_gender_code" ) ] ) ,
	margin = 2
)
sum( saeb_df[ , "provider_enumeration_year" ] )

tapply(
	saeb_df[ , "provider_enumeration_year" ] ,
	saeb_df[ , "provider_gender_code" ] ,
	sum 
)
quantile( saeb_df[ , "provider_enumeration_year" ] , 0.5 )

tapply(
	saeb_df[ , "provider_enumeration_year" ] ,
	saeb_df[ , "provider_gender_code" ] ,
	quantile ,
	0.5 
)
sub_saeb_df <- subset( saeb_df , provider_business_practice_location_address_state_name = 'CA' )
mean( sub_saeb_df[ , "provider_enumeration_year" ] )
var( saeb_df[ , "provider_enumeration_year" ] )

tapply(
	saeb_df[ , "provider_enumeration_year" ] ,
	saeb_df[ , "provider_gender_code" ] ,
	var 
)
t.test( provider_enumeration_year ~ individual , saeb_df )
this_table <- table( saeb_df[ , c( "individual" , "is_sole_proprietor" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		provider_enumeration_year ~ individual + is_sole_proprietor , 
		data = saeb_df
	)

summary( glm_result )
library(dplyr)
saeb_tbl <- tbl_df( saeb_df )
saeb_tbl %>%
	summarize( mean = mean( provider_enumeration_year ) )

saeb_tbl %>%
	group_by( provider_gender_code ) %>%
	summarize( mean = mean( provider_enumeration_year ) )
dbGetQuery( db , "SELECT COUNT(*) FROM " )
}
