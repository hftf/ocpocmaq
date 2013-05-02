(* ::Package:: *)
(* 2012-12-06 *)

Clear[c,H,Q,P0,M,k,i,m,\[Sigma],j,r];
MyPrint[p_] := 0;
format[n_] := NumberForm[N@n,{2,2}];

c=10; (* Number of categories *)
(*H=Table[Subscript[h, i],{i,1,c}];*)
H=Table[1/c,{i,1,c}]; (* Every category starts with an equal probability *)
(*m=14;
Q=Table[Subscript[q, i],{i,1,m}]; *)
Q={};
K=Min[c-1,4];
A[i_,m_]:=UnitStep[m-K-i+1](m-K-i+1) 1/ If[m<=K,1,Sum[d,{d,1,m-K}]];
(*A[i_,m_]:=i/m;*)
(*A[i_,m_]:=UnitStep[i-(m+2)]1/m*)
(*K=1;
A[i_,m_]:=Piecewise[{{1,m<=K}},UnitStep[i-K-1]1/(m-K)];*)
SetAttributes[A,Listable];
P0=H/Total[H];
M=20; (* Number of tossups *)

For [m=1,m<=M,m++,
 PP=H/Total[H]; (* Renormalize PP as copy of H *)
 MyPrint["m: ",m];
 MyPrint["PP: ",PP];
 AA=A[Range[m-1],m-1];
 MyPrint["AA: ",AA];

 For[i=m-1,i>=1,i--,
  (*Print["i: ",i," m: ",m," AA[i]: ",AA[[i]]];*)
  PP[[ Q[[i]] ]]*=AA[[i]];
 ];

 PP/=Total[PP];
 Print["PP after loop: ",format@PP];
 Print[PP];
 
 r=RandomReal[];
 (* r=1; *)
 \[Sigma]=PP[[1]];
 j=1;
 While[\[Sigma]<r,
  j++;\[Sigma]+=PP[[j]];
 ];
 AppendTo[Q,j];
 H[[j]]-=Min[1/M, H[[j]]];

 Print["Q: ",Q];
 Print["H: ",H];
 Print["_______"];
 (* j is the new category *)
 (* j = RandomVariate[EmpiricalDistribution[PP->Range[Length[PP]]]]*)
]


N[A[Range[#],#]&[7],{\[Infinity],3}]


A[Range[5],5]


A
