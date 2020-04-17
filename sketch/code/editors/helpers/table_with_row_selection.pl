/*
definition of class tableWithRowSelection

2006 Anders Bouwer, Universiteit van Amsterdam,
*/


:-pce_begin_class(tableWithRowSelection,
		  table,
		  "Table with row selection"
		 ).

variable(currentRow,table_row*,both). %currently selected row in the table
variable(changedContents, bool, both). %status of the table: changed or not

%%
initialise(T):->
	"Initialise the table" ::
	T->+initialise,
        send(T, slot, changedContents, @off).




clear(T):->
	"Delete all rows" ::

        % delete_rows:       From    , To,     , KeepGraphicals
        send(T, delete_rows, @default, @default, @off),

        % reset currentRow
        send(T, currentRow, @nil).


:-pce_end_class.
