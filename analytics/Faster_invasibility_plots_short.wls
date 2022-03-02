#!/usr/bin/env wolframscript

SetDirectory@Directory[];
Clear["Global`*"];

types={"private","by group","public"};

setupallequations[p_,q_, ua_,up_,nu1_]:=(
  eps=(1-ua)*(1-up)+ua up;
  nu2 = 1.0 - nu1;

  A21 = A12;
  A22 = A11;
  B21 = B12;
  B22 = B11;

  (* observer view - donor intent - probgood *)
  pgoodGC=eps;
  pgoodGD=ua;
  pgoodBC=p(eps-ua)+q(1-eps-ua)+ua;
  pgoodBD=q(1-2ua)+ua;

  (* generation equations for reputation relations *)
  gbul1 = nu1 g11Z  + nu2 g21Z;
  gbul2 = nu1 g12Z  + nu2 g22Z;
  gstar1 = nu1 g11S  + nu2 g21S;
  gstar2 = nu1 g12S  + nu2 g22S;
  g11a1 = nu1 g11Z g11Z + nu2 g21Z g21Z;
  g11b1 = gbul1 - g11a1;
  g11c1 = gbul1 - g11a1;
  g11d1 = 1 - gbul1 - gbul1 + g11a1;
  g12a1 = nu1 g12Z g11Z + nu2 g22Z g21Z;
  g12b1 = gbul2 - g12a1;
  g12c1 = gbul1 - g12a1;
  g12d1 = 1 - gbul2 - gbul1 + g12a1;
  g21a1 = nu1 g11Z g12Z + nu2 g21Z g22Z;
  g21b1 = gbul1 - g21a1;
  g21c1 = gbul2 - g21a1;
  g21d1 = 1 - gbul1 - gbul2 + g21a1;
  g22a1 = nu1 g12Z g12Z + nu2 g22Z g22Z;
  g22b1 = gbul2 - g22a1;
  g22c1 = gbul2 - g22a1;
  g22d1 = 1 - gbul2 - gbul2 + g22a1;
  g11a2 = nu1 g11S g11Z + nu2 g21S g21Z;
  g11b2 = gbul1 - g11a2;
  g11c2 = gstar1 - g11a2;
  g11d2 = 1 - gbul1 - gstar1 + g11a2;
  g12a2 = nu1 g12S g11Z + nu2 g22S g21Z;
  g12b2 = gbul2 - g12a2;
  g12c2 = gstar1 - g12a2;
  g12d2 = 1 - gbul2 - gstar1 + g12a2;
  g21a2 = nu1 g11S g12Z + nu2 g21S g22Z;
  g21b2 = gbul1 - g21a2;
  g21c2 = gstar2 - g21a2;
  g21d2 = 1 - gbul1 - gstar2 + g21a2;
  g22a2 = nu1 g12S g12Z + nu2 g22S g22Z;
  g22b2 = gbul2 - g22a2;
  g22c2 = gstar2 - g22a2;
  g22d2 = 1 - gbul2 - gstar2 + g22a2;
  g11a3 = nu1 g11Z g11S + nu2 g21Z g21S;
  g11b3 = gstar1 - g11a3;
  g11c3 = gbul1 - g11a3;
  g11d3 = 1 - gstar1 - gbul1 + g11a3;
  g12a3 = nu1 g12Z g11S + nu2 g22Z g21S;
  g12b3 = gstar2 - g12a3;
  g12c3 = gbul1 - g12a3;
  g12d3 = 1 - gstar2 - gbul1 + g12a3;
  g21a3 = nu1 g11Z g12S + nu2 g21Z g22S;
  g21b3 = gstar1 - g21a3;
  g21c3 = gbul2 - g21a3;
  g21d3 = 1 - gstar1 - gbul2 + g21a3;
  g22a3 = nu1 g12Z g12S + nu2 g22Z g22S;
  g22b3 = gstar2 - g22a3;
  g22c3 = gbul2 - g22a3;
  g22d3 = 1 - gstar2 - gbul2 + g22a3;
  g11a4 = nu1 g11S g11S + nu2 g21S g21S;
  g11b4 = gstar1 - g11a4;
  g11c4 = gstar1 - g11a4;
  g11d4 = 1 - gstar1 - gstar1 + g11a4;
  g12a4 = nu1 g12S g11S + nu2 g22S g21S;
  g12b4 = gstar2 - g12a4;
  g12c4 = gstar1 - g12a4;
  g12d4 = 1 - gstar2 - gstar1 + g12a4;
  g21a4 = nu1 g11S g12S + nu2 g21S g22S;
  g21b4 = gstar1 - g21a4;
  g21c4 = gstar2 - g21a4;
  g21d4 = 1 - gstar1 - gstar2 + g21a4;
  g22a4 = nu1 g12S g12S + nu2 g22S g22S;
  g22b4 = gstar2 - g22a4;
  g22c4 = gstar2 - g22a4;
  g22d4 = 1 - gstar2 - gstar2 + g22a4;

  g11Zpriv=g11a1 pgoodGC + g11b1 pgoodGD + g11c1 pgoodBC + g11d1 pgoodBD;
  g12Zpriv=g12a1 pgoodGC + g12b1 pgoodGD + g12c1 pgoodBC + g12d1 pgoodBD;
  g21Zpriv=g21a1 pgoodGC + g21b1 pgoodGD + g21c1 pgoodBC + g21d1 pgoodBD;
  g22Zpriv=g22a1 pgoodGC + g22b1 pgoodGD + g22c1 pgoodBC + g22d1 pgoodBD;
  g11Zpub=gbul1 pgoodGC + (1-gbul1)pgoodBD;
  g12Zpub=gbul2 pgoodGC + (1-gbul2)pgoodBD;
  g21Zpub=gbul1 pgoodGC + (1-gbul1)pgoodBD;
  g22Zpub=gbul2 pgoodGC + (1-gbul2)pgoodBD;
  g11Zind=g11a2 pgoodGC + g11b2 pgoodGD + g11c2 pgoodBC + g11d2 pgoodBD;
  g12Zind=g12a2 pgoodGC + g12b2 pgoodGD + g12c2 pgoodBC + g12d2 pgoodBD;
  g21Zind=g21a2 pgoodGC + g21b2 pgoodGD + g21c2 pgoodBC + g21d2 pgoodBD;
  g22Zind=g22a2 pgoodGC + g22b2 pgoodGD + g22c2 pgoodBC + g22d2 pgoodBD;
  g11SZind=g11a3 pgoodGC + g11b3 pgoodGD + g11c3 pgoodBC + g11d3 pgoodBD;
  g12SZind=g12a3 pgoodGC + g12b3 pgoodGD + g12c3 pgoodBC + g12d3 pgoodBD;
  g21SZind=g21a3 pgoodGC + g21b3 pgoodGD + g21c3 pgoodBC + g21d3 pgoodBD;
  g22SZind=g22a3 pgoodGC + g22b3 pgoodGD + g22c3 pgoodBC + g22d3 pgoodBD;
  g11SZpriv=g11a4 pgoodGC + g11b4 pgoodGD + g11c4 pgoodBC + g11d4 pgoodBD;
  g12SZpriv=g12a4 pgoodGC + g12b4 pgoodGD + g12c4 pgoodBC + g12d4 pgoodBD;
  g21SZpriv=g21a4 pgoodGC + g21b4 pgoodGD + g21c4 pgoodBC + g21d4 pgoodBD;
  g22SZpriv=g22a4 pgoodGC + g22b4 pgoodGD + g22c4 pgoodBC + g22d4 pgoodBD;
  g11SZpub=gstar1 pgoodGC +(1-gstar1) pgoodBD;
  g12SZpub=gstar2 pgoodGC +(1-gstar2) pgoodBD;
  g21SZpub=gstar1 pgoodGC +(1-gstar1) pgoodBD;
  g22SZpub=gstar2 pgoodGC +(1-gstar2) pgoodBD;

  pStype1=fZA1(1-pQ)+(1-fZA1)(1-pR);
  pIndiv1=fZA1 pQ+(1-fZA1)pR;
  pStype2=fZA2(1-pQ)+(1-fZA2)(1-pR);
  pIndiv2=fZA2 pQ+(1-fZA2)pR;

  (* general equation to solve for equilibrium reputations; we use FindRoot which is fast but requires initial guess *)

  ng11Z := g11Z;
  ng12Z := g12Z;
  ng21Z := g21Z ;
  ng22Z := g22Z ;
  ng11S := g11S;
  ng12S := g12S;
  ng21S := g21S;
  ng22S := g22S;

  (*coop11[r_] := (1-r) ng11Z + r ng11S ;
  coop12[r_] := (1-r) ng21Z + r ng21S;
  coop21[r_] := (1-r)ng12Z + r ng12S;
  coop22[r_] := (1-r) ng22Z + r ng22S;*)
)

Clear[nu];nu=.5;
setupallequations[0,1,ua,up,nu]

nsoln[pQQ_,pRR_,uaa_,upp_,Atype_, Btype_,fZAA_]:=Module[{A11,A12,B11,B12,A21,A22,B21,B22},
  A11 = KroneckerDelta[Atype, 1] + KroneckerDelta[Atype, 2];
  A12 = KroneckerDelta[Atype, 2];
  B11 = KroneckerDelta[Btype, 1] + KroneckerDelta[Btype, 2];
  B12 = KroneckerDelta[Btype, 2];
  A21 = A12;
  A22 = A11;
  B21 = B12;
  B22 = B11;

  FindRoot[{
  g11Z == pStype1 ((1 - A11)g11Zpriv+ A11 g11Zpub)+pIndiv1 g11Zind,
  g12Z == pStype1 ((1 - A21)g12Zpriv+ A21 g12Zpub)+pIndiv1 g12Zind,
  g21Z == pStype2 ((1 - A12)g21Zpriv+ A12 g21Zpub)+pIndiv2 g21Zind,
  g22Z == pStype2 ((1 - A22)g22Zpriv+ A22 g22Zpub)+pIndiv2 g22Zind,
  g11S == pStype1  g11SZind+ pIndiv1 ((1 - B11)g11SZpriv+ B11 g11SZpub),
  g12S == pStype1  g12SZind+ pIndiv1 ((1 - B21)g12SZpriv+ B21 g12SZpub),
  g21S == pStype2  g21SZind+ pIndiv2 ((1 - B12)g21SZpriv+ B12 g21SZpub),
  g22S == pStype2  g22SZind+ pIndiv2 ((1 - B22)g22SZpriv+ B22 g22SZpub)
  }/.{ ua->uaa, up->upp,fZA1->fZAA,fZA2->fZAA,pQ->pQQ,pR->pRR},{{g11Z,.49},{g12Z,.49},{g21Z,.49},{g22Z,.49},
  {g11S,.49},{g12S,.49},{g21S,.49},{g22S,.49}}
  ]
]

canZQinvadeZR[pQQ_, pRR_,uaa_,upp_,Atype_,Btype_,nu1_,a_,b_,c_]:=Module[{A11,A12,B11,B12,A21,A22,B21,B22,\[CapitalDelta]g11ZQR,\[CapitalDelta]g12ZQR,\[CapitalDelta]g21ZQR,\[CapitalDelta]g22ZQR},
  A11 = KroneckerDelta[Atype, 1] + KroneckerDelta[Atype, 2];
  A12 = KroneckerDelta[Atype, 2];
  B11 = KroneckerDelta[Btype, 1] + KroneckerDelta[Btype, 2];
  B12 = KroneckerDelta[Btype, 2];
  A21 = A12;
  A22 = A11;
  B21 = B12;
  B22 = B11;
  \[CapitalDelta]g11ZQR=(pRR-pQQ)((1 - A11)g11Zpriv+ A11 g11Zpub)+(pQQ-pRR)g11Zind/.{ ua->uaa, up->upp,pQ->pQQ,pR->pRR};
  \[CapitalDelta]g12ZQR=(pRR-pQQ)((1 - A11)g12Zpriv+ A11 g12Zpub)+(pQQ-pRR)g12Zind/.{ ua->uaa, up->upp,pQ->pQQ,pR->pRR};
  \[CapitalDelta]g21ZQR=(pRR-pQQ)((1 - A11)g21Zpriv+ A11 g21Zpub)+(pQQ-pRR)g21Zind/.{ ua->uaa, up->upp,pQ->pQQ,pR->pRR};
  \[CapitalDelta]g22ZQR=(pRR-pQQ)((1 - A11)g22Zpriv+ A11 g22Zpub)+(pQQ-pRR)g22Zind/.{ ua->uaa, up->upp,pQ->pQQ,pR->pRR};

  (b(1-pRR)(nu1^2 \[CapitalDelta]g11ZQR+nu1(1-nu1)\[CapitalDelta]g12ZQR+(1-nu1)nu1 \[CapitalDelta]g21ZQR+(1-nu1)^2 \[CapitalDelta]g22ZQR)-(pQQ-pRR)(c(nu1(gstar1-gbul1)+(1-nu1)(gstar2-gbul2))-a))
]

(* Clear[secantmethod,secantmethodbyinterval]; *)
secantmethod[f_,x0_,x1_,tol_:10^-10,iter_:50,stepbystep_:False]:=Module[{fx0,fx1,fx2,xx0,xx1,xx2,dir},
  xx0=x0;
  xx1=x1;
  fx0=N@Evaluate[f[xx0]];
  fx1=N@Evaluate[f[xx1]];
  If[stepbystep==True,Print["start from: x0 = ",xx0,", x1 = ",xx1]];
  (* check that we haven't found the root yet *)
  If[Abs[fx0]<tol,
  If[stepbystep==True,Print["\tYou already found the root at x = ",xx0]];
  dir=If[N@Evaluate[f[xx0+tol]]>fx0,"Up","Down"];
  Return[{xx0,dir}]
  ];
  If[Abs[fx1]<tol,
  If[stepbystep==True,Print["\tYou already found the root at x = ",xx1]];
  dir=If[N@Evaluate[f[xx1-tol]]<fx1,"Up","Down"];
  Return[{xx1,dir}]
  ];
  (* check that f(x0) and f(x1) have different signs *)
  If[fx0*fx1>=0,If[stepbystep==True,Print["\tNeed f(x0)*f(x1)<0; check x0 and x1 values"]];Return[]];
  (* iteratively find root *)
  i=0;
  While[i<=iter,
  (* update guessed root *)
  xx2=(xx0*fx1-xx1*fx0)/(fx1-fx0);
  fx2=N@Evaluate[f[xx2]];
  If[Abs[fx2]<tol,
  (* found the root *)
  If[stepbystep==True,Print["\tFound root x = ",xx2," after ",i," iterations"]];
  dir=If[fx1>fx0,"Up","Down"];
  Return[{xx2,dir}],
  (* haven't found the root; update x0 and x1 *)
  If[stepbystep==True,Print["\titeration ",i,": f(",xx0,") = ",fx0,", f(",xx1,") = ",fx1]];
  If[fx0*fx2>0,xx0=xx2;fx0=fx2,
  If[fx1*fx2>0,xx1=xx2;fx1=fx2,Print["!!! check x values !!!: x0 = ",xx0,", x1 = ",xx1,", x2 = ",xx2]]
  ];
  i++
  ];
  ]
]

secantmethodbyinterval[f_,xmin_:0.,xmax_:1.,xintervals_:10,tol_:10^-10,iter_:50,stepbystep_:False]:=Module[{breaks,sol,sollist,soltable},
  sollist={};
  breaks=Sort@Flatten@{N@Subdivide[xmin,xmax,xintervals],0.01,0.99};
  If[stepbystep==True,Print[breaks]];
  soltable=Table[
  sol=secantmethod[f,breaks[[j]],breaks[[j+1]],tol,iter,stepbystep];
  sollist=AppendTo[sollist,sol];
  {breaks[[j]],breaks[[j+1]],sol},
  {j,1,xintervals}
  ];
  If[stepbystep==True,Print[soltable//MatrixForm]];
  sollist=Chop[Cases[sollist,{_?NumericQ,_}],tol*10];
  Return[sollist]
]

(* Clear[flatten,singularbcsweep,singularasweep,singularupsweep,singularuasweep,singulartablebc,singulartablea,singulartableup,singulartableua]; *)

flatten[data_]:=Flatten[Table[Table[
  Flatten@{data[[i,1]],data[[i,2,j]]},{j,1,Length@data[[i,2]]}],{i,1,Length@data}],1
]

plotUpDown[data_,xmin_,xmax_,ymin_,ymax_,Atype_,Btype_,xlabel_:"b/c"]:=Module[{dataUp,dataDown,pUp,pDown},
  dataUp=Cases[data,{_,_,"Up"}][[;;,1;;2]];
  dataDown=Cases[data,{_,_,"Down"}][[;;,1;;2]];
  (*
  pUp=ListPlot[dataUp,PlotMarkers->Style[\[FilledCircle],Purple,5],PlotRange->{{xmin,xmax},{ymin-0.01,ymax+0.01}}];
  pDown=ListPlot[dataDown,PlotMarkers->Style[\[EmptyCircle],Gray,5],PlotRange->{{xmin,xmax},{ymin-0.01,ymax+0.01}}];
  *)
  pUp=ListPlot[dataUp,PlotMarkers->{Graphics[{Purple, Disk[{0, 0}]}], 0.02},PlotRange->{{xmin,xmax},{ymin-0.01,ymax+0.01}}];
  pDown=ListPlot[dataDown,PlotMarkers->{Graphics[{Gray, Thickness[Small], Circle[{0, 0}]}], 0.02},PlotRange->{{xmin,xmax},{ymin-0.01,ymax+0.01}}];
  Show[{pUp,pDown},
  PlotRange->{Automatic,{ymin-0.11,ymax+0.01}},
  AxesOrigin->{Automatic,-0.1},
  AxesLabel->{Style[xlabel,Black],Style["\!\(\*SuperscriptBox[\(p\), \(*\)]\)",Black]},
  AspectRatio->1,
  ImageSize->{200,200},
  PlotLabel->Style["Reputations "<>types[[Atype+1]]<>"\nStereotypes "<>types[[Btype+1]],Black]
  ]
]

singulartablebc[nu_,up_,ua_,a_,c_,Atype_,Btype_,pRmin_:0.,pRmax_:1.,pRintervals_:10,bmin_:1.,bmax_:3.,bstep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{ff,sols,data},
  data=Table[
  ff[pR_]:=canZQinvadeZR[0.,pR,up,ua,Atype,Btype,nu,a,bb,c]/.nsoln[0.,pR,up,ua,Atype,Btype,0];
  sols=secantmethodbyinterval[ff,pRmin,pRmax,pRintervals,tol,iter,stepbystep];
  {bb/c,sols},
  {bb,bmin,bmax,bstep}
  ];
  data=flatten[data]
]

singulartablea[nu_,up_,ua_,b_,c_,Atype_,Btype_,pRmin_:0.,pRmax_:1.,pRintervals_:10,amin_:0.,amax_:1.,astep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{ff,sols,data},
  data=Table[
  ff[pR_]:=canZQinvadeZR[0.,pR,up,ua,Atype,Btype,nu,aa,b,c]/.nsoln[0.,pR,up,ua,Atype,Btype,0];
  sols=secantmethodbyinterval[ff,pRmin,pRmax,pRintervals,tol,iter,stepbystep];
  {aa/c,sols},
  {aa,amin,amax,astep}
  ];
  data=flatten[data]
  ]

singulartableup[nu_,ua_,a_,b_,c_,Atype_,Btype_,pRmin_:0.,pRmax_:1.,pRintervals_:10,upmin_:0.,upmax_:1.,upstep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{ff,sols,data},
    data=Table[
    ff[pR_]:=canZQinvadeZR[0.,pR,upp,ua,Atype,Btype,nu,a,b,c]/.nsoln[0.,pR,upp,ua,Atype,Btype,0];
    sols=secantmethodbyinterval[ff,pRmin,pRmax,pRintervals,tol,iter,stepbystep];
    {upp,sols},
    {upp,upmin,upmax,upstep}
    ];
    data=flatten[data]
  ]

singulartableua[nu_,up_,a_,b_,c_,Atype_,Btype_,pRmin_:0.,pRmax_:1.,pRintervals_:10,uamin_:0.,uamax_:1.,uastep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{ff,sols,data},
    data=Table[
    ff[pR_]:=canZQinvadeZR[0.,pR,up,uaa,Atype,Btype,nu,a,b,c]/.nsoln[0.,pR,up,uaa,Atype,Btype,0];
    sols=secantmethodbyinterval[ff,pRmin,pRmax,pRintervals,tol,iter,stepbystep];
    {uaa,sols},
    {uaa,uamin,uamax,uastep}
    ];
    data=flatten[data]
]

singularbcsweep[nu_,up_,ua_,a_,c_,pRmin_:0.,pRmax_:1.,pRintervals_:10,bmin_:1.,bmax_:3.,bstep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{data,dataUp,dataDown,pUp,pDown,ff,sols},
    Grid@Table[
    data=singulartablebc[nu,up,ua,a,c,Ai,Bi,pRmin,pRmax,pRintervals,bmin,bmax,bstep,tol,iter,stepbystep];
    plotUpDown[data,bmin,bmax,pRmin,pRmax,Ai,Bi,"b/c"],
    {Ai,{2,1,0}},{Bi,{0,1,2}}
    ]
]

singularasweep[nu_,up_,ua_,b_,c_,pRmin_:0.,pRmax_:1.,pRintervals_:10,amin_:0.,amax_:1.,astep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{data,dataUp,dataDown,pUp,pDown,ff,sols},
    Grid@Table[
    data=singulartablea[nu,up,ua,b,c,Ai,Bi,pRmin,pRmax,pRintervals,amin,amax,astep,tol,iter,stepbystep];
    plotUpDown[data,amin,amax,pRmin,pRmax,Ai,Bi,"a"],
    {Ai,{2,1,0}},{Bi,{0,1,2}}
    ]
]

singularupsweep[nu_,ua_,a_,b_,c_,pRmin_:0.,pRmax_:1.,pRintervals_:10,upmin_:0.,upmax_:1.,upstep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{data,dataUp,dataDown,pUp,pDown,ff,sols},
    Grid@Table[
    data=singulartableup[nu,ua,a,b,c,Ai,Bi,pRmin,pRmax,pRintervals,upmin,upmax,upstep,tol,iter,stepbystep];
    plotUpDown[data,upmin,upmax,pRmin,pRmax,Ai,Bi,"\!\(\*SubscriptBox[\(u\), \(p\)]\)"],
    {Ai,{2,1,0}},{Bi,{0,1,2}}
    ]
]

singularuasweep[nu_,up_,a_,b_,c_,pRmin_:0.,pRmax_:1.,pRintervals_:10,uamin_:0.,uamax_:1.,uastep_:0.04,tol_:10^-10,iter_:100,stepbystep_:False]:=Module[{data,dataUp,dataDown,pUp,pDown,ff,sols},
  Grid@Table[
  data=singulartableua[nu,up,a,b,c,Ai,Bi,pRmin,pRmax,pRintervals,uamin,uamax,uastep,tol,iter,stepbystep];
  plotUpDown[data,uamin,uamax,pRmin,pRmax,Ai,Bi,"\!\(\*SubscriptBox[\(u\), \(a\)]\)"],
  {Ai,{2,1,0}},{Bi,{0,1,2}}
  ]
]

Print["Finished setting up all functions"]

directory="./stypes_plots/";
CreateDirectory[directory];

AbsoluteTiming[
  
  (* setup for all cases *)
  ppRmin = 0.; ppRmax = 1.; ppRintervals = 10;
  ttol=10^-10;iiter=100;sstepbystep=False;

  (* sweep b *)
  nuval=0.5;upval=.02;uaval=.02;
  aval=.3;cval=1.;bval=2;
  bbmin=1.;bbmax=3.;bbstep=0.05;

  {time,results} = singularbcsweep[nuval, upval, uaval, aval, cval, ppRmin, ppRmax, ppRintervals, bbmin, bbmax, bbstep, ttol, iiter, sstepbystep]//AbsoluteTiming;
  Print["Done in : ",time," seconds"];
  Export[directory<>"a_"<>ToString[aval]<>"_b_varied_c_"<>ToString[cval]<>"_ints_"<>ToString[ppRintervals]<>".pdf",results];

  (* sweep a *)
  nuval = 0.5; upval = .02; uaval = .02;
  (*aval=.3;*)cval = 1.; bval = 2;
  aamin = 0.01; aamax = 1.; aastep = 0.025;

  {time,results} = singularasweep[nuval, upval, uaval, bval, cval, ppRmin, ppRmax, ppRintervals, aamin, aamax, aastep, ttol, iiter, sstepbystep]//AbsoluteTiming;
  Print["Done in : ",time," seconds"];
  Export[directory<>"a_varied_b_"<>ToString[bval]<>"_c_"<>ToString[cval]<>"_ints_"<>ToString[ppRintervals]<>".pdf",results];

  (* sweep up *)
  nuval = 0.5;(*upval=.02;*)uaval = .02;
  aval = .3; cval = 1.; bval = 2;
  uupmin = 0.01; uupmax = 0.5; uupstep = 0.01;

  {time,results} = singularupsweep[nuval, uaval, aval, bval, cval, ppRmin, ppRmax, ppRintervals, uupmin, uupmax, uupstep, ttol, iiter, sstepbystep]//AbsoluteTiming;
  Print["Done in : ",time," seconds"];
  Export[directory<>"up_varied_a_"<>ToString[aval]<>"_b_"<>ToString[bval]<>"_c_"<>ToString[cval]<>"_ints_"<>ToString[ppRintervals]<>".pdf",results];

  (* sweep ua *)
  nuval = 0.5; upval = .02;(*uaval=.02;*)
  aval = .3; cval = 1.; bval = 2;
  uuamin = 0.01; uuamax = 0.5; uuastep = 0.01;

  {time,results} = singularuasweep[nuval, upval, aval, bval, cval, ppRmin, ppRmax, ppRintervals, uuamin, uuamax, uuastep, ttol, iiter, sstepbystep]//AbsoluteTiming;
  Print["Done in : ",time," seconds"];
  Export[directory<>"ua_varied_a_"<>ToString[aval]<>"_b_"<>ToString[bval]<>"_c_"<>ToString[cval]<>"_ints_"<>ToString[ppRintervals]<>".pdf",results];
]


