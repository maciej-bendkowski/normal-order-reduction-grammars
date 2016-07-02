#!/usr/local/bin/WolframScript -script
(* Computes the relative density of R_n in CL. Requires the *)
(* R_0,...,R_n grammars to be available in i.in files. *)
(* Densities are computed using the simplified Szego Lemma. *)
(* Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl> *)

n = ToExpression[$ScriptCommandLine[[2]]];
Do[Subscript[R,i][z_] = ToExpression[ReadString[StringJoin[ToString[i],".in"]]], {i,0,n,1}];

F[z_] = (1-Sqrt[1-8*z])/(2*z);
P[z_] = Subscript[R,n][z] /. z -> (z/8);
G[z_] = P[z] /. z -> (1-z^2);
H[z_] = G[z] /. Sqrt[z^2] -> z;
d = Simplify[(H'[0])/(-4)];
Print[N[d]]
