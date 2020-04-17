/* Tabstack which manages the size of the tabButtons */


:-pce_begin_class(
		    managed_tab_stack,
		    tab_stack,
		    "A tabstack which manages the size of the tabButtons"
		 ).


initialise(T, Tabs: object ...) :->
    flatten([initialise, Tabs], InitialiseAndTabs),
    InitialiseAndTabsCall =.. InitialiseAndTabs,
    send_super(T, InitialiseAndTabsCall).

update_tabnames(_T) :->
    get(@app?modelsTabs, keys, TabsChain),
    get(TabsChain, size, NumberOfTabs),
    %new(Sizes, new(chain)),
    %send(TabsChain, forall,
    %message(Sizes, append, @arg1?label_size?width)
    %),
    %chain_list(SizesChain, Sizes),
    %sum_list(Sizes, SumSizes),
    TotalSize = 340,
    (
	NumberOfTabs > 0 ->
	ceiling(TotalSize / (NumberOfTabs * 9), TabLength)
    ;
	ceiling(TotalSize, TabLength)
    ),
    send(TabsChain, for_all,
	message(@arg1, update_tabname, TabLength)
    ).

:-pce_end_class.

