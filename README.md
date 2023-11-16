
# Benchmarking OMOP CDM: Network analysis code

To perform a network analysis study, the code in this repository was developed through the partnership of two ICU quality registries. These are the Critical Care Asia Africa (CCAA) Network, based in England, and the [National Intensive Care Evaluation (NICE)], based in the Netherlands.

## Goal

The goal of the study was to perform a benchmarking analysis with results that could be compared. To compare benchmarking results, analysis should be done similarly. However, the difference in our database structure has thus far proven to be a large obstacle. Enter [OHDSI]'s [OMOP Common Data Model (CDM)]. A way to standardize the structure of a database according to strict guidelines. Both registries implemented the database model through a systematic ETL with various quality checks.

## Usage

The script *00_setup.R* will run the whole benchmarking analysis. However, there are two files that need customizing before the script will run completely. These are created and opened by *00_setup.R* if not available.
They are created from their example files: [example_concepts.csv] and [example_connection_parameters.R]. How to fill them in properly we explain here. 

### connection_parameters.R

The file contains multiple variables to which empty strings are assigned. Populate these strings in with your connection parameters.  Mandatory to fill in are the *dataset_name*, *driver*, *host*, *dbname*, *schema*, *user*, and *password* variables. The *port* variable is only needed for PostgreSQL databases. 
A preview of the example file:

    # ------------------------------------------------------------
    # Connection parameters
    # These parameters are needed to connect to the OMOP database
    # ------------------------------------------------------------
    # The name of your dataset or quality registry (eg. CCAA, NICE, etc.)
    dataset_name <- ""
    
    # The driver of your database (eg. PostgreSQL, SQL Server, etc.)
    driver <- ""
    
    # The hostname of the server your database is on
    host <- ""
    
    # (OPTIONAL) The port your server is available on
    port <- ""
    
    # The name of your database (POSSIBLY DIFFERENT FROM YOUR DATA SET'S NAME!)
    dbname <- ""
    
    # The schema used to store your OMOP CDM tables in
    schema <- ""
    
    # The username to gain access to your database
    user <- ""
    
    # The password to gain access to your database
    password <- ""


	 
### {dataset_name}_concepts.csv

The name of this file depends on what you entered in *connection_parameters.R*. 
Countries which use a comma as decimal separator (e.g. most European countries): open the file in notepad and add `sep=,` on the first line of the document, or replace all commas ( , ) with semicolons ( ; ).
	
The file contains a table with eight columns: 
	   
| score |  short_name | table | concept_id | omop_variable | concept_id_value | name_of_value | Note |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|

NOTE: The `score`-column and `short_name`-column should not be changed, unless specified in notes.
	
**If** the entries in the `name_of_value`-column with *"comorbidity"* as a value in the `short_name`-column do not correspond to the diagnoses used by your registry to calculate the APACHE II score: 
- Remove these columns. 
- Add your diagnoses to the `name_of_value`-column beneath the last row. 
- Fill in their `score`-column as *"APACHE II"* and the `short_name`-column as *"comorbidity"*.
		
#### Filling the table
Search for the variables listed in `name_of_value`-column in your database and fill in for each variable:
| Column name | Expected value |
|--:|:--|
| table | The names of the OMOP CDM tables in which the variable is stored. |
| concept_id | The concept_id assigned to the variable. |
| omop_variable | The column name in which their values are stored.<br /> Likely column names are:<br /> *"condition_concept_id"*, *"procedure_concept_id"*<br /> *"value_as_number"*, or *"value_as_concept_id"*.<br /> The latter two are the important to the calculations. |
| (**If** *"value_as_concept_id"* was filled in) <br /> concept_id_value | The concept_id assigned to the variable. |

###  Further customization

If criteria are used to exclude certain patients from your calculations, add these as a function to the [apache_ii_prob_function.R] file


[apache_ii_prob_function.R]:https://github.com/aasiyahrashan/benchmarking-OMOP/blob/d2c23ccc6e926fe8ec0ad4ad3db26f5363271ad0/analysis/apache_ii_prob_function.R

[example_concepts.csv]:https://github.com/aasiyahrashan/benchmarking-OMOP/blob/d2c23ccc6e926fe8ec0ad4ad3db26f5363271ad0/analysis/example_concepts.csv

[example_connection_parameters.R]:https://github.com/aasiyahrashan/benchmarking-OMOP/blob/d2c23ccc6e926fe8ec0ad4ad3db26f5363271ad0/analysis/example_connection_parameters.R

[OHDSI]: https://ohdsi.github.io/TheBookOfOhdsi

[OMOP Common Data Model (CDM)]:https://ohdsi.github.io/CommonDataModel

[National Intensive Care Evaluation (NICE)]: https://www.stichting-nice.nl
