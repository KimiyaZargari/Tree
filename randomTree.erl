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
	{Val, Parent, []}.
	
	
mkTree(List)->
	Len = length(List),	
	NewList = randomize(List, Len/2, Len),
	Root = mkNode(hd(NewList), 'nil'),
	{Tree, _} = mkTree(Root, tl(NewList)),
	Size = randomTree: size(Tree),
	if  Size =:= 1->
		mkTree(List);
	true->
		Tree
	end.
		
mkTree(Root, List) ->
	Len = length(List),
	NumOfChildren = rand:uniform(Len +1) - 1,
	if NumOfChildren > 0 ->
	addChildren(Root, List, NumOfChildren, [],[], NumOfChildren);
	NumOfChildren =:= 0 ->
	{Root, List}
	end.
	
		
addChildren(Parent, List, Num, ChList, _ , Num2) when (Num > 0)->	
	 addChildren(Parent, tl(List), Num - 1, [mkNode(hd(List), Parent)|ChList], [], Num2);
		
addChildren(Parent, List,0, ChList,UpdatedChList, Num2) when Num2 > 0->			
	{CurrentChild, NewList} = mkTree(hd(ChList), List),
	addChildren(Parent, NewList,0, tl(ChList), [CurrentChild|UpdatedChList],Num2 - 1);	
	
addChildren(Parent, List,_, _, UpdatedChList,_) ->
	NewParent = {element(1, Parent), element(2, Parent), UpdatedChList},
	{NewParent, List}.
	
	
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
	{_,_,Children} = Tree,
	size(Children, 1).
size(Children, Ans)->		
	Len = length(Children),
	childIteration(Children, 1, Ans + Len, Len).
			
childIteration(Children, N, Ans, Len) when (N =< Len)-> 
	Node = lists: nth(N, Children),
	{_, _, NewChildren} = Node,
	NewAns = size(NewChildren, Ans), 
	childIteration(Children, N + 1 ,NewAns, Len);
childIteration(_, _, Ans, _) ->
	Ans.