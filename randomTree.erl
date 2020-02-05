%%----------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%%----------------------------------------------------------------------
%% File:       tree
%% Author       : Kimiya Zargari
%% Description  : module for a tree data structure
%% 
%% Modules used : ...
%% 
%%----------------------------------------------------------------------

%% @doc Describe the functionality of this module.
%%
-module(randomTree).
-export([mkTree/1, size/1]).

mkNode(Val, Parent) ->
	{Val, Parent}.
	
	
mkTree(List)->
	Len = length(List),	
	NewList = randomize(List, Len/2, Len),
	Root = mkNode(hd(NewList), 'nil'),
	mkTree(Root, tl(NewList)).
	
mkTree(Root, List) ->
	Len = length(List),
	NumOfChildren = rand:uniform(Len +1) - 1,
	addChildren(Root, List, NumOfChildren, []).
	
	
addChildren(Parent, List, Num, ChList) when Num > 0->	
	addChildren(Parent, tl(List), Num -1, [mkNode(hd(List), Parent)|ChList]);
	
addChildren(Parent, List,_, ChList) ->
	%{Val, P} = Parent,	
	{NewChList, NewList} = mkTreeForEachChild(ChList, length(ChList), List, Parent, []),
	NewParent = erlang : append_element(Parent, NewChList),
	{NewParent, NewList}.
	
	
mkTreeForEachChild(ChList,Len, List, Parent, NewChList)->
	if (Len > 0) and (List =/= [])->
		{NewChild, NewList} = mkTree(hd(ChList), List) ,
		mkTreeForEachChild(tl(ChList), Len - 1, NewList, Parent, [NewChild|NewChList]);
	true -> {NewChList, List} 
	end.
	 

randomize(List, N, Len)->
	if N >= 1 ->
		LT = swap(List, rand: uniform(Len), rand: uniform(Len)),
		randomize(LT, N-1, Len);
	true -> List
	end.
		
swap(L, I, J)->
	{L2,[F|L3]} = lists:split(I - 1 ,L),
	L4 = L2 ++ [lists: nth( J, L)] ++ L3,
	{L5, [_|L6]} = lists:split(J - 1,L4),
	L5 ++  [F] ++ L6.
	


size(Tree)->
	size(Tree, 1).
size(Tree, Ans)->
	{_,_, Children} = Tree,
	CLen = length(Children),
	if CLen =/= 0 ->
		childIteration(Children, 1, Ans)
	end,
	Ans.
	
	
childIteration(Children, N, Ans)->
	Len = length(Children),
	if N =< length(Children)->
		size(lists: nth(N, Children), Ans + Len), 
		childIteration(Children, N + 1,Ans + Len )
	end.