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
	if Tree == 'nil' -> Root;
	true-> Tree
	end.
	
	
mkTree(Root, List) ->
	Len = length(List),
	NumOfChildren = rand:uniform(Len +1) - 1,
	if NumOfChildren > 0 ->
	addChildren(Root, List, NumOfChildren, [], 1,  NumOfChildren);
	NumOfChildren =:= 0 ->
	{'nil', List}
	end.
	
		
addChildren(Parent, List, Num, ChList,1, Num2) when (Num > 0) and (List =/= [])->	
	 addChildren(Parent, tl(List), Num -1, [mkNode(hd(List), Parent)|ChList],1, Num2);
		
addChildren(Parent, List,_, ChList, Num3,  Num2) when (Num3 =< Num2) and (ChList =/= []) and (List =/= []) ->			
	{CurrentChild, NewList} = mkTree(lists: nth(Num3, ChList), List),	
	addChildren(Parent, NewList,0, [CurrentChild| tl(ChList)], Num3 + 1, Num2);	
addChildren(Parent, List,_, ChList, _, _) ->
	NewParent = {element(1, Parent), element(2, Parent), ChList},
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
	size(Tree, 1).
size(Tree, Ans)->
	{_,_,Children} = Tree,
	CLen = length(Children),
	if CLen =/= 0 ->
		childIteration(Children, 1, Ans, CLen);
	true -> Ans
	end.
	
	
childIteration(Children, N, Ans, Len) when (N =< Len)->
		size(lists: nth(N, Children), Ans + Len), 
		childIteration(Children, N + 1,Ans + Len, Len );
childIteration(_, _, Ans, _) ->
	Ans.