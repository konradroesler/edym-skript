#import "@preview/physica:0.9.3": *
#import "@preview/commute:0.2.0": node, arr, commutative-diagram
#import "@preview/cetz:0.3.1": *
#import "@preview/cetz-plot:0.1.0": plot, chart

#import "utils.typ": *
#import "template.typ": uni-script-template

#show: doc => uni-script-template(
  title: [Vorlesungsskript],
  author: [Konrad Rösler],
  module-name: [Elektrodynamik],
  doc
)

= Worum geht es in der Elektrodynamik?

#bold[In der klassischen Mechanik:]

fundamentale Konzepte: Länge, Zeit, #underline[Masse]

$arrow.long$ Trägheit + Gravitation

Newtonsche Bew. gl.: $arrow(F) = m dot arrow(a)$, $arrow(F) = G dot (M dot m)/r^2 arrow(e)_r$ wobei $underbrace(arrow(r),"Ort") underbrace((t), "Zeit") ==> arrow(a) = (d^2 arrow(r))/(d t^2) = dot.double(arrow(r))$  

#align(center, italic[(Abbildung eines Massepunktes in 2D)])

Lagrange-Funktion:

$arrow.long$ Wirkung
$
S = integral d t L(arrow(r), dot(arrow(r)))
$

$N$ Teilchen $arrow(r)_i (t), i = 1, ..., N$
$
L(arrow(r)_i, dot(arrow(r))_i) = sum_(i = 1)^N 1/2 m_i abs(dot(arrow(r))_i)^2 - V(arrow(r)_i) \
V(arrow(r)_i) = - G/2 limits(sum_(i, j = 1)^N)_(i != j) (m_i m_j)/abs(arrow(r)_i - arrow(r)_j)
$

#bold[Neue fundamentale Größe:]

- elektrische Ladung $q$ (positiv oder negativ)
- gequantelt mit #underline[Elementarladung $e$]
$
&q = n dot e, n in ZZ \
&q > 0 "(Proton, Positron," n = +1")" \
&q < 0 "(Elektron," n = -1")"
$

#bold[Coulomb-Gesetz:] Kraft zwischen elektrisch geladenen Teilchen
$
arrow(F)_1 = k dot q_1 q_2 (arrow(r)_1 - arrow(r)_2)/abs(arrow(r)_1 - arrow(r)_2)^3 = - arrow(F)_2
$

#align(center, italic[(Abbildung Coloumbgesetz zwischen zwei Teilchen)])

$q_1 q_2 > 0$ (Ladungen haben dasselbe Vorzeichen) $==>$ abstoßend

$q_1 q_2 < 0$ (Ladungen haben verschiedene Vorzeichen) $==>$ anziehend 

#bold[Was ist $k$? (Einheitensysteme)]

#underline[1) Gausssche System:] $k = 1$

#underline[2) SI System:] $k = 1/(4 pi epsilon_0)$

#underline[3) Heavyside-Lorentz-System:] $k = 1/(4 pi)$

Umrechnen: SI $->$ Gauss: $e_0 = 1/(4 pi)$, SI $->$ Heavyside: $epsilon_0 = 1$

#bold[Zusätzliche Realität:]

magenetische Felder, elektromagnetische Wellen

$
--> #bold[Feldtheorie] "(Maxwell's Theorie, erstes Beispiel)"
$
$arrow(x)_i (t), quad i = 1, ..., N quad$ diskrete Zahl an freiheitsgrade $= 3N$

$-->$ Elektrodynamik $arrow(E)(t, arrow(x)), arrow(B)(t, arrow(x))$

Betrachte ein Kraftfeld, erzeugt durch $N$ Punktladungen $q_i, 1 = 1, ..., N$ wirkend auf eine Testladung $abs(q) << abs(q_i)$
$
==> arrow(F) = q arrow(E)(arrow(x)), quad arrow(E)(arrow(x)) = 1/(4 pi epsilon_0) sum_(i = 1)^N q_i (arrow(x) - arrow(x)_i)/abs(arrow(x) - arrow(x)_i)^3 \ 
"das Elektrische Feld" 
$
eine fixierte Ladung an $arrow(x)_1$
$
arrow(E)(arrow(x) #text(fill: red)[$, t$]) = 1/(4 pi epsilon_0) q_1 (arrow(x) - arrow(x)_1 #text(fill: red)[$(t)$])/abs(arrow(x) - arrow(x)_1 #text(fill: red)[$(t)$])^3
$
#align(center, italic[(Abbildung einer Ladung als Punktteilchen)])

Diese (naive) Zeitabhängigkeit ist empirisch falsch und im Widerspruch zur (speziellen) Relativitätstheorie (SR)

$-->$ Maxwell's Theorie, kompatibel mit SR

== Plan der Vorlesung

1. Wiederholung
  - Euklidische Geometrie im $RR^3$, Vektoranalysis (Differentialformen)
2. Spezielle Relativitätstheorie
  - (Psuedo-) Euklidische Geometrie des Minkowski-Raum $RR^(3, 1)$
3. Maxwell's Theorie
4. Anwendungen
  1. Elektrostatik
  2. Magnetostatik
  3. Elektro- und Magnetostatik in Materie

#pagebreak()

= Wiederholung: Vektoranalysis im $RR^3$

Der euklidische $RR^3$: $arrow(x) = arrow(r) = (x^1, x^2, x^3) = (x^i), quad i = 1,2,3$

#underline[Metrik:]
$
ip(arrow(x)_1 - arrow(x)_2, arrow(x)_1 - arrow(x)_2) = abs(arrow(x)_1 - arrow(x)_2)^2 = sum_(i = 1)^3 (x_1^i - x_2^i)(x_1^i - x_2^i)
$

Geometrie invariant unter Rotationen
$
x^i --> x'^i = sum_(j = 1)^3 R^i_j x^j underbrace(=, "Einstein Konvention") R^i_j x^j
$
$
abs(arrow(x))^2 = delta_(i j) x^i x^j quad "wobei" delta_(i j) = cases(1 quad i = j, 0 quad i != j)
$
$
abs(arrow(x)')^2 &= delta_(i j) x'^i x'^j = delta_(i j) R^i_k x^k R^j_l x^l \
&= (delta_(i j) R^i_k R^j_l) x^k x^l = abs(arrow(x))^2 = delta_(k l) x^k x^l
$
$
==> delta_(i j) R^i_k R^j_l = delta_(k l)
$
Matrix-Notation: $R = (R^i_j), bb(1) = (delta_(i j))$
$
delta_(k l) = R^i_k delta_(i j) R^j_l ==> bb(1) = R^T R \
==> det(R) = plus.minus 1
$
Rotationsgruppe: $"SO"(3): det(R) = +1$

---

Im $RR^3$ hat man das #bold[Kreuz-Produkt]

Epsilon-Tensor / Levi-Civita-Symbol

$
epsilon^(i j k), epsilon_(i j k): quad epsilon^(1 2 3) = - epsilon^(2 1 3) = epsilon^(2 3 1) = 1
$
total antisymmetrisch, da $epsilon^(1 1 2) = 0 = - epsilon^(1 1 2)$

$==>$ invariant unter Rotation / $"SO"(3)$

$
epsilon^(i j k) --> R^i_m R^j_n R^K_l epsilon^(m n l) underbrace(=, det(R) = 1) epsilon^(i j k)
$
Im euklidischen $RR^3$ darf man nur folgende Objekte benutzen:
$
delta_(i j), delta^(i j), epsilon^(i j k), epsilon_(i j k)
$
#underline[Skalarprodukt:] $ip(arrow(x), arrow(y)) = delta_(i j) x^i y^j$ 

#underline[Kreuzprodukt:] $arrow(u) times arrow(v) = - arrow(v) times arrow(u), (arrow(u) times arrow(v))^i := delta^(i l) epsilon_(l j k) u^j v^k$

#underline[Skalare/Funktionen auf $RR^3$:] $F = F(arrow(x)) in RR$

#underline[Vektorfeld auf RR^3:] $arrow(V) = arrow(V)(arrow(x))$

#underline[Gradient:] $diff_i := pdv(,x^i)$, Skalar $-->$ Vektor 
$
arrow(grad)F = "grad" F, quad ("grad" F)^i = delta^(i j) diff_j F = (pdv(F,x^1), pdv(F,x^2), pdv(F,x^3))
$

#underline[Divergenz:] Vektor $-->$ Skalar
$
"div" arrow(V) &= div arrow(V) = diff_i V^i \
&= pdv(V^1,x^1) + pdv(V^2,x^2) + pdv(V^3,x^3)
$

#underline[Rotation:] Vektor $-->$ Vektor
$
"rot" arrow(V) = curl arrow(V) ==> ("rot" V)^i = epsilon^(i j k) diff_j V_k 
$

$
"Skalare" -->^"grad" "Vektoren" -->^"rot" "Vektoren" -->^"div" "Skalare"
$
Identitäten (Kettenkomplex):
$
"rot" compose "grad" = 0 \
"div" compose "rot" = 0
$

#bold[Differentialformen im $RR^3$:]

- #underline[0-Formen:] Skalar

- #underline[1-Formen:] "dual" zu Vektoren, $A_i (arrow(x))$
$
"[Im Euklidischen: " V_i (arrow(x)) = delta_(i j) V^j (arrow(x)) "]"
$

- #underline[2-Formen:] Antisymmetrischer Tensor
$
B_(i j) (arrow(x)) = - B_(i j) (arrow(x))
$

- #underline[3-Formen:] $C_(i j k) (arrow(x)) = - C_(i k j) (arrow(x)) = ...$ (wie Levi-Civita)

#underline[Effiziente indexfreie Notation:] Basis-Elemente $d x^i$

- 1-Form: $A = A_i dd(x^i)$
- 2-Form: $B = 1/2 B_(i j) dd(x^i,x^j,p:and)$
- 3-Form: $C = 1/3! C_(i j k) dd(x^i,x^j,x^k,p:and)$

wobei $dd(x^i,x^j,p:and) = - dd(x^j,x^i,p:and)$

#bold[Wedge Product:]
$
A and B = (A_i dd(x^i)) and (1/2 B_(j k) dd(x^j,x^k,p:and)) = 1/2 A_i B_(j k) dd(x^i,x^j,x^k,p:and) "(3-Form)"
$

$p$-Form $A$, $q$-Form $B$ 
$
==> A and B /* space */ "ist" (p+q)-"Form" \
A and B = (-1)^(p q) B and A "(gradiert Kommutativ)" \
(A and B) and C = A and (B and C) "(assoziativ)"
$

#underline[deRham Differential:]
$
upright("d") :&Omega^k->Omega^(k+1) \
&A |-> dd(A):= diff_i dd(x^i) and A
$
Beispiel: 
$
dd(A) &= dd(A_j d x^j) \
&= diff_i d x^i and (A_j dd(x^j)) \
&= diff_i A_j dd(x^i) and dd(x^j) \ 
&= 1/2 (diff_i A_j - diff_j A_i) underbrace(dd(x^i,x^j,p:and), -dd(x^j,x^i,p:and))
$

$Omega^p:$ $p$-Formen, $upright("d"): Omega^p --> Omega^(p+1)$, $upright(d)^2 = 0$ (Übungsaufgabe)

#underline[Hodge Operator:]
$
star: Omega^p <--> Omega^(3-p) \
star: Omega^1 <--> Omega^(2) \
star: Omega^3 <--> Omega^(0) \
$
$A$ ist 1-Form, $B$ ist 2-Form, $C$ ist 3-Form
$
star A &= 1/2 epsilon_(i j)^k A_k dd(x^i,x^j,p:and) \
star B &= 1/2 epsilon_i^(j k) B_(j k) dd(x^i) \
star C &= 1/3! epsilon^(i j k) C_(i j k)
$
Wir erweitern das Diagramm von vorher:
#let omegas = ()
#(for i in range(4) {omegas.push(node((1,i),$Omega^#i$))})
#(for i in range(3) {omegas.push(arr((1,i),(1,i+1),"d"))})
#align(center)[#commutative-diagram(
  node((0, 0), "Skalare"),
  node((0, 1), "Vektoren"),
  node((0, 2), "Vektoren"),
  node((0, 3), "Skalare"),
  arr((0,0), (0,1), "grad"),
  arr((0,1), (0,2), "rot"),
  arr((0,2), (0,3), "div"),
  arr((0,0), (1,0), $id$, "bij"),
  arr((0,1), (1,1), $delta_(i j)$, "bij"),
  arr((0,2), (1,2), $star compose ♡$, "bij"),
  arr((0,3), (1,3), $star$, "bij"),
  ..omegas,
)]
Dieses Diagramm kommutiert. (Alle Pfade, die zwei Punkte verbinden, sind  äquivalent.)
$
upright(d)^2 = 0 <==> "rot" compose "grad" = 0, "div" compose "rot" = 0
$

#text(size: 14pt)[#bold[Wiederholung]]

Beim letzten Mal: $arrow(x) = (x^i) in RR^3, space i,j = 1,2,3$

Invarianten der euklidischen Geometrie
$
&delta_(i j) = delta^(i j) = cases(1 quad i = j, 0 quad "sonst") \ 
&epsilon_(i j k) = epsilon^(i j k) = plus.minus 1
$

#bold[Skalarprodukt/Euklidische Metrik:]

$
ip(arrow(v), arrow(w)) = arrow(v) dot arrow(w) = delta_(i j) v^i w^j in RR \ 
equiv v^i w_i = v_i w^i
$

#bold[Kreuzprodukt:]
$
(arrow(v) times arrow(w))^i := epsilon^(i j k) v_j w_k = epsilon^(i j k) delta_(j n) delta_(k l) v^n w^l
$

Skalare und Vektoren im $RR^3$ $<-->$ Differentialformen im $RR^3$

#underline[1-Form:] $(A_i), A = A_i dd(x^i), A_i = delta_(i j) A^j$ ($<-->$ #underline[Vektor])

#underline[2-Form:] $B = 1/2 B_(i j) dd(x^i, x^j, p:and), B_(i j) = epsilon_(i j k) B^k$ ($<-->$ #underline[Vektor])

#underline[3-Form:] $C = 1/3! C_(i j k) dd(x^i, x^j, x^k, p:and)$

Hodge-dual zu Skalar: $F(arrow(x)) = 1/3! epsilon^(i j k) C_(i j k) (arrow(x))$

#line(length: 100%)

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  [
    Sei $F: RR^3 -> RR$ ein Skalarfeld, dann ist $grad F$ senkrecht auf der Fläche $F^(-1)({0})$. 
    #grid(
      columns: (2fr,1fr), gutter: 20pt,
      [
        *Beispiel:* $F(arrow(x)) := abs(arrow(x))^2 - R^2, R = "const"$ 
        $
        F^(-1)({0})=:S^2_R space ("2-Sphäre") wide (grad F)^i = delta^(i j) delta_j F
        $
        $
        diff_j F(arrow(x)) &= diff_j (abs(arrow(x))^2) = diff_j (delta_(k l) x^k x^l) \
        &= delta_(k l) (diff_j x^k) x^l + delta_(k l) x^k (diff_j x^l) \
        &= 2 delta_(k l) (diff_j x^k) x^l \
        &= 2 delta_(j l) x^l
        $
        $
        ==> (grad F)^i = delta^(i j)2delta_(j l)x^l = 2 dot x^i \
        grad F = 2 dot arrow(x) 
        $
      ],
      [
        #v(2cm)
        #canvas({
        import draw: *
    
        circle((),radius:1,name:"sphere")
        line(("sphere"),(1.8,-0.5),mark:(end:">",fill:black))
        content((),$grad F$,anchor:"west",padding:0.1)
        set-style(stroke:(paint:gray,dash:"dashed"))
        circle((0,0),radius:1.5)
        circle((),radius:0.5)
      })
      ]
    )
    #bold[Beweis:]

    Parametrisierung der Fläche $F = 0$, Parameter $sigma^alpha, alpha = 1,2 <==> F(arrow(x)(sigma)) = 0$
    $
    pdv(arrow(x),sigma^1) "Tangentialvektor entlang" sigma^1 \
    pdv(arrow(x),sigma^2) "Tangentialvektor entlang" sigma^2
    $
    $
    ip(pdv(arrow(x), sigma^alpha), grad F) = delta_(i j) pdv(x^i, sigma^alpha) (grad F)^j = delta^(j k) diff_k F \ 
    = delta_i^(space k) pdv(x^i, sigma^alpha) diff_k F = pdv(x^i, sigma^alpha) diff_i F =^"Kettenregel" pdv(,sigma^alpha) (F(arrow(x)(sigma))) = 0 \
    ==> grad F "senkrecht auf Tangentialvektoren"
    $
  ]
)

#text(size: 13pt)[#bold[Integralsätze:]]

#underline[Linienintegral:] Kurve im $RR^3$: $C$, $arrow(x)(s) = x^i (s), s in [s_0, s_1] subset RR$

1-Form kann entlang einer Kurve integriert werden 
$
"1-Form" <--> "Kurven"
$
#underline[1-Form:]
$
A = A_i (arrow(x)) dd(x^i) \ 
integral_C A equiv integral_C A_i dd(x^i) := integral_(s_0)^(s_1) A_i (arrow(x)(s)) dv(x^i, s) dd(s)
$
Euklidische Metrik:
$
A <--> arrow(A) wide integral A_i dd(x^i) = integral delta_(i j) A^i dd(x^i)  \ 
==> integral A_i dd(x^i)  = integral arrow(A) dot dd(arrow(x)) 
$
Fall $A$ exakt $A = d phi.alt, A_i = diff_i phi.alt$
$
integral_C A = integral_C d phi.alt = integral_(s_0)^(s_1) diff_i phi.alt(arrow(x)(s)) dv(x^i, s) dd(s) =^"Kettenregel" integral_(s_0)^(s_1) dif/dd(s) phi.alt(arrow(x)(s)) dd(s) = phi.alt(arrow(x)(s_1)) - phi.alt(arrow(x)(s_0)) \ 
integral_(phi.alt) dd(phi.alt) = phi.alt(arrow(x)(s_1)) - phi.alt(arrow(x)(s_0))
$
$==>$ hängt nur von Endpunkten ab!

$==>$ geschlossene Kurve:
#align(center)[
  #canvas({
    import draw: *
    circle((),radius:0.06,fill:black)
    content((),$arrow(x)(s_1)=arrow(x)(s_2)$,anchor:"north-east",padding:0.1)
    let (a, b, c) = ((0, 0), (-1, 4.5), (4, -1.5))
    bezier(a, a, b, c)
    content((4,00.5),$
    integral.cont_C dd(phi.alt) = 0
    $)
  }),
]
Für konservatives Kraftfeld $arrow(A) = grad phi.alt$
$
==> integral_C grad phi.alt dot dd(arrow(x)) = phi.alt(arrow(x)(s_1)) - phi.alt(arrow(x)(s_0))
$

#underline[Flächenintegral:]
$
"2-Form" <--> "Fläche"
$

Parametrisierung $Sigma$: $arrow(sigma) = arrow(x)(u, v), sigma^alpha = (u, v) in D subset RR^2$
$
B = 1/2 B_(i j) dd(x^i, x^j, p:and), wide dd(x^i) = pdv(x^i, sigma^alpha) dd(sigma^alpha) = pdv(x^i,u) dd(u) + pdv(x^i, v) dd(v) \ 
integral_Sigma B = 1/2 dot integral_Sigma B_(i j) dd(x^i, x^j, p:and) = 1/2 integral_D B_(i j) (arrow(x)(sigma)) pdv(x^i, sigma^alpha) pdv(x^j, sigma^beta) dd(sigma^alpha, sigma^beta, p:and) \ 
==> integral_Sigma B := 1/2 integral_D B_(i j) (arrow(x)(u, v)) (pdv(x^i,u) pdv(x^j,v) - pdv(x^i, v) pdv(x^j, u)) dd(u,v) \
integral_Sigma B = integral_D B_(i j) (arrow(x)(u, v)) pdv(x^i,u) pdv(x^j,v) dd(u,v) = integral_Sigma arrow(V) dot dd(arrow(Sigma))
$
wobei $dd(arrow(Sigma)) := (pdv(arrow(x),v) times pdv(arrow(x),u)) dd(u, v)$

#underline[Volumenintegral:] 
$
"3-Formen" <--> "Volumen"
$
$
C = 1/3! C_(i j k) dd(x^i, x^j, x^k, p:and)
$
$
integral_V C = 1/3! integral e^(i j k) C_(i j k) (arrow(x)) dd(x)^3
$

#bold[Integralsätze/Stokes von Stokes:]

Sei $M$ eine Kurve/Fläche/Volumen und $diff(M)$ der Rand: 

- $M = C$ Kurve: $diff M$ sind die Endpunkte
- $M = Sigma$ Kurve: $diff M$ ist die Randkurve 
- $M = V$ Kurve: $diff M$ ist die Oberfläche von $V$

#box(width: 100%, inset: 0.3cm, stroke: 0.5pt, radius: 5pt, [
  #bold[Stokes Theorem]

  Sei $A$ eine $p$-Form und $M$ $(p+1)$-dimensional. Dann gilt 
  $
  integral_(diff M) A = integral_M dd(A)
  $
])

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
$M$: Kurve (1-dimensional), $A = phi.alt$ (0-Form)

Hauptsatz der Differential und Integralrechnung:
$
integral_M dd(phi.alt) = phi.alt(arrow(x)(s_1)) - phi.alt(arrow(x)(s_0)) = integral_(diff M) phi.alt \
"für" diff M = {arrow(x)(s_1), arrow(x)(s_0)}
$
  ]
)

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
$M$: Fläche, 1-Form, $A = A_i dd(x^i)$, Parametrisierung: $sigma^alpha = (u, v) in D 
$
$
dd(A) = 1/2 (diff_i A_j - diff_j A_i) dd(x^i, x^j, p:and) \
$
$
integral_M dd(A) &= integral_D (diff_i A_j - diff_j A_i) pdv(x^i,u) pdv(x^j,v) dd(u,v) \
&= integral_D (pdv(,u) A_j (arrow(x)(u, v)) pdv(x^j,v) - pdv(,v) A_j (arrow(x)(u, v)) pdv(x^i,u)) dd(u, v) \
&=limits(integral_D pdv(,u)(A_j (arrow(x)(u,v)) pdv(x^j,v)) dd(u,v))_arrow.b-integral_D pdv(,v)(...)dd(u,v)=integral_(diff M) A space checkmark \
$
$
integral_(v_0)^(v_1) A_j (arrow(x)(u_1, v) pdv(x^j,v) dd(v) - integral_(v_0)^(v_1) A_j (arrow(x)(u_0, v)) pdv(x^j,v) dd(v)
$
#align(center)[#canvas({
  import draw: *
  bezier((0.7*1.5,1.5*1),(1.2*1.5,1.5*0.7),(0.7*1.5,1.5*0.7))
  bezier((1.2*1.5,1.5*0.7),(1.7*1.5,1.5*1.5),(1.9*1.5,1.5*0.7))
  bezier((1.7*1.5,1.5*1.5),(1*1.5,1.5*1.6),(1.6*1.5,1.5*2))
  bezier((1*1.5,1.5*1.6),(0.7*1.5,1.5*1),(0.65*1.5,1.5*1.35))
  content((1.8,1.8),$M$)

  set-origin((-5,0.8))
  
  line((-0.5,0),(3,0),mark:(end:">",fill:black))
  content((),$u$,anchor:"west",padding:0.15)
  line((0,-0.5),(0,2.5),mark:(end:">",fill:black))
  content((),$v$,anchor:"west",padding:0.15)
  rect((0.5,0.5),(2,1.5),stroke:(dash:"dashed"))
  content((0.5,-0.35),$u_0$)
  line((0.5,-0.1),(0.5,0.1))
  content((2,-0.3),$u_1$)
  line((2,-0.1),(2,0.1))
  content((-0.35,0.5),$v_0$)
  line((-0.1,0.5),(0.1,0.5))
  content((-0.35,1.5),$v_1$)
  line((-0.1,1.5),(0.1,1.5))

  line((4,1),(5,1),mark:(start:"|",end:">",fill:black))
  content((rel:(-0.6,0.3)),$x$)
})]
#v(1cm)
 ]
)
#pagebreak()
#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
$M$: Volumen, $B$ 2-Form, $B = 1/2 B_(i j) dd(x^i, x^j, p:and)$
$
==> "3-Form" space dd(B) = 1/2 diff_i B_(j k) dd(x^i, x^j, x^k, p:and) = 1/2 epsilon^(i j k) diff_i B_(j k) dd(x)^3 \
==> dd(B) = diff_i V^i dd(x)^3 quad "mit" quad V^i := 1/2 epsilon^(i j k) B_(j k) \
==> integral_V dd(B) = integral_V diff_i V^i dd(x)^3 = integral_M div arrow(V) dd(x)^3 =^"Gauss" integral.cont_(diff V) arrow(V) dot dd(arrow(Sigma)) = integral_(diff M) B
$])
Konsistenz mit $"div" compose "rot" = 0, ..., "d"^2 = 0$

$M = diff M' ==> diff M = 0 quad [diff diff = 0]$

$A = dd(C) ==> integral_M dd(A) = integral_(diff M) dd(C)=integral_(diff (diff M)) C=0 ==> dd(A) = upright("d")^2 C=0 quad [upright("d")upright("d")=0]$

#bold[Koordinatenwechsel:]
$
integral_V F(arrow(x)) dd(x)^3 = 1/3! integral_V F(arrow(x)) epsilon_(i j k) dd(x^i, x^j, x^k, p:and)
$
$-->$ neue Koordinaten $va(w): va(x) = va(x)(va(w))$
$
x_1 = x_1(w_1, w_2, w_3), w_1 = w_1(x_1, x_2, x_3) \
dots.v 
$
Es gilt 
$
dd(x^i) = pdv(x^i, w^j) dd(w^j)
$
$
integral_V F(arrow(x)) dd(x)^3 = integral_V F(va(x)(va(w))) 1/3! epsilon_(i j k) pdv(x^i, w^l) pdv(x^j, w^m) pdv(x^k, w^n) underbrace(dd(w^l, w^m, w^n, p:and), = epsilon^(l m n) dd(w^1, w^2, w^3, p:and))
$
$= det(J)$

#bold[Jacobi-Matrix:] 
$
tensor(J, -i, +j) = (pdv(x^j, w^i))
$
$
==> dd(x)^3 = abs(det(J)) dd(x)^3
$

#pagebreak()

= Spezielle Relativitätstheorie

#bold[Raumzeit:] Raum und Zeit vereinigt in einem vierdimensionalen Raum

Punkt der Raumzeit: #bold[Ereignis] (etwas, das zu einem festen Zeitpunkt an einem Ort stattfindet)

Literaturempfehlung: Robert Geroch, General Relativity from A to B

#align(center)[#canvas({
  import draw: *
  set-transform(none)
  rotate(x:30deg,y:-40deg,z:-90deg)
  set-style(fill:black)
  line((-1,0),(3,0),mark:(end:">"))
  content((),$t$,anchor:"south-west",padding:0.1)
  line((0,-1),(0,4),mark:(end:">"))
  content((),$x$,anchor:"north",padding:0.1)
  line((0,0,-1),(0,0,3),mark:(end:">"))
  content((),$y$,anchor:"north",padding:0.1)

  let (x1,x2) = (1.5,1.3)
  line((0,x1,x2),(0,0,x2),stroke:(dash:"dashed"))
  content((),$x_0$,anchor:"south-east",padding:0.1)
  line((0,x1,x2),(0,x1,0),stroke:(dash:"dashed"))
  content((),$y_0$,anchor:"south-west",padding:0.09)
  set-style(stroke:blue,fill:blue)
  line((-2,x1,x2),(4.5,x1,x2))
  line((-2,x1,x2),(4.5,x1,x2),mark:(end:">",pos:25%))
  set-style(fill:none)
  bezier((-1.3,3.7,1.3),(1,3.05,0.5),(-1,2.5,0.5))
  bezier((1,3.05,0.5),(4,3.05),(1.87,2.9,-1))
  bezier((1,3.05,0.5),(4,3.05),(1.87,2.9,-1),mark:(end:">",pos:50%))

  content((4,7),[Raumzeit als $RR^4$])
})]

Struktur der Raumzeit?

#v(1cm)
#grid(
  columns: (2fr,1fr), align: (left,center),
  [
#bold[Aristotelische Raumzeit:] 

Folgende Fragen sind bedeutungsvoll
1. Finden zwei Ereignisse am selben Ort statt?
2. Finden zwei Ereignisse zur selben Zeit statt?

Antworten:
1. Ereignisse liegen in der selben senkrechten Ebene
1. Ereignisse liegen in der selben 3D-Ebene

Es gilt das Prinzip der "absoluten Ruhe".
  ],
  [
    #canvas({
      import draw: *
      set-transform(none)
      rotate(x:80deg,y:-20deg,z:0deg)
      rect((-2,-2,0),(2,2,0))
      content((),$t=0$,anchor:"west",padding:1)
      rect((-2,-2,2),(2,2,2))
      content((),$t=3$,anchor:"west",padding:1)
      set-style(mark:(end:">",start:"o"),fill:blue,stroke:blue)
      line((-0.75,0,0),(-0.75,0,3.5))
      line((0.75,0,0),(0.75,0,3.5))
      content((),remark[Weltlinien (feste Orte)],anchor:"south-west",padding:0.2)
      set-style(mark:(start:">",end:none,fill:gray),fill:none,stroke:gray)
      bezier((2,2,3.4),(2.2,0,3.6),(2.2,0,3.4))
      set-transform(none)
      bezier((-1.8,0.5),(-2.5,1.3),(-1.8,1))
      content((),remark[3-Ebene],anchor:"north-east")
    })
  ]
)
#pagebreak()

#grid(
  columns: (1.7fr,1fr), align: (left,center), gutter: 20pt,
  [
    #bold[Galileische Raumzeit:]

    "Zwei Ereigniss finden zur selben Zeit statt." hat eine absolute Bedeutung, das Prinzip der absoluten Ruhe gilt jedoch nicht.

    Im Allgemeinen macht es keinen Sinn zu fragen was der räumliche Abstand #underline[zwischen zwei Ereignissen] $p$ und $q$ ist.

    Aber: Der räumliche Abstand #underline[zur selben Zeit] \ (in der $x_1$-$x_2$-$x_3$-Ebene) ist absolut.

    $==>$ Newtonsches Gravitationsgesetz ist kompatibel mit Galilei [$V(r) op(tilde)$ $r$: Distanz zu festem Punkt]
  ],
  [
    #v(0.8cm)
    #canvas({
  import draw: *
  set-transform(none)
  rotate(x:50deg,y:-10deg,z:-90deg)
  set-style(fill:black)
  line((0,0),(3,0),mark:(end:">"))
  content((),$t$,anchor:"south-west",padding:0.1)
  line((0,0),(0,4),mark:(end:">"))
  content((),$x_1$,anchor:"north")
  line((0,0),(0,0,4),mark:(end:">"))
  content((),$x_2$,anchor:"north")

  let quader(a,b,c,d,e,f,g,h) = {
    line(a,b);line(b,c);line(c,d);line(d,a);
    line(a,e);line(b,f);line(c,g);line(d,h);
    line(e,f);line(f,g);line(g,h);line(h,e);
  }
  for i in range(5) {
    quader((i*0.4,0,-i/3+0.5),(i*0.4,2,-i/3+0.5),(i*0.4,2,-i/3+0.5+2),(i*0.4,0,-i/3+0.5+2),((i+1)*0.4,0,-i/3+0.5),((i+1)*0.4,2,-i/3+0.5),((i+1)*0.4,2,-i/3+0.5+2),((i+1)*0.4,0,-i/3+0.5+2))
  }
  set-transform(none)
  content((-0.5,-4),[Galileitransformation \ ist Scheerung in $x_1$-$x_2$-$x_3$ Ebene:])
})
  ]
)
#v(1cm)

#bold[Minkowski Raum:]

$RR^4: x^mu = (c t, x, y, z), mu = 0, 1, 2, 3 equiv (x^0, x^i), i = 1, 2,3$. Dabei ist $c$ die Lichtgeschwindigkeit [$c=1$ in bestimmen Einheiten, z.B. räumliche Koordinaten in Lichtjahren und $t$ in Jahren]

#bold[Minkowski-Metrik:] "pseudo-euklidische Metrik"
$
eta_(mu nu) := diagonalmatrix(1,-1,-1,-1,fill:0) = eta^(mu nu)
$

Raumzeit-Intervall/Abstand zwischen zwei Punkten mit Koordinatendifferenz $Delta x^mu$
$
Delta s^2 &:= eta_(mu nu) Delta x^mu Delta x^v \
&= (Delta x^0)^2 - (Delta x^1)^2 - (Delta x^2)^2 - (Delta x^3)^2 \
&= c^2 (Delta t)^2 - (Delta arrow(x))^2
$
Infinitesimal: $dd(s)^2 = eta_(mu nu) dd(x^mu, x^nu) = c^2 dd(t)^2 - (dd(va(x)))^2$

#bold[Vektoren im Minkowski-Raum $RR^(3, 1)$:]

$V^mu$: 4er-Vektor/"kontravarianter" Vektor

Minkowski-Norm: $V^2 = ip(V, V) = eta_(mu, nu) V^mu V^nu equiv V_mu V^mu$ wobei "index herunter gezogen" zum "kontravarianten" Vektor ("co-Vektor")
$
V_mu := eta_(mu nu) V^nu = (v^0, -v^1, -v^2, -v^3)
$
Hochziehen von Indizes: $V^mu = eta^(mu nu) V_nu$

Notationsoptionen:
$
ip(V, W) = eta_(mu nu) V^mu W^nu = V_mu W^mu = V^mu W_mu
$

#grid(
  columns: (1fr,1fr),align: center,
  [
    #v(1cm)
    $x^mu x_mu = 0$ heißt #bold[lichtartig]
    
    $x^mu x_mu > 0$ heißt #bold[zeitartig]
    
    $x^mu x_mu < 0$ heißt #bold[raumartig] 
  ],
  canvas({
  import draw: *
  let a = 2
  let cone_length = a/1.8
  set-style(fill:black,mark:(symbol:">"))
  set-transform(none)
  rotate(x:60deg,y:40deg,z:0deg)
  line((-a,0,0),(a,0,0))
  content((),$x^1$,anchor:"west",padding:0.1)
  line((0,-a,0),(0,a,0))
  content((),$x^2$,anchor:"north",padding:0.1)
  line((0,0,-a),(0,0,a))
  content((),$x^0$,anchor:"south",padding:0.1)
  let zylinder = {
    line((0,0),(-cone_length,0,cone_length),mark:none)
    line((0,0),(cone_length,0,cone_length),mark:none)
    line((0,0),(0,cone_length,cone_length),mark:none)
    line((0,0),(0,-cone_length,cone_length),mark:none)
    circle((0,0,cone_length),radius:cone_length,fill:none)
  }
  zylinder
  rotate(y:180deg)
  zylinder
})
)

Dies ist eine Klassifizierung von Punkten im Minkowski-Raum.

== Lichtstrahlen und Uhren

#bold[Postulat 1:] Weltlinien von Lichtstrahlen sind Kurven (= Geraden) auf der Oberfläche des Lichtkegels.


#bold[Postulat 2:] Weltlinien von massiven Objekten/Beobachtern sind zeitartige Kurven

#bold[Postulat 3:] Die Zeit, die ein Beobachter entlang seiner Weltlinie "misst", ist 
$
T = 1/c sqrt((Delta s)^2) = sqrt((Delta t)^2 - ((Delta va(x))^2)/c^2)
$

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
    Physikalische Interpretation von zwei Ereignissen $p$ und $q$ mit raumartigen Intervall? 

    #align(center)[#canvas({
      import draw: *
      set-style(fill:black,radius:0.04,padding:0.15)
      let (r,p,s,q)=((0,0),(0.25,1),(0.75,3),(2.4,1.1))
      line((-0.25,-1),(0.85,3.4))
      circle(r)
      content((),$r$, anchor:"north-west")
      circle(p)
      content((),$p$, anchor:"west")
      circle(s)
      content((),$s$, anchor:"south-west")
      circle(q)
      content((),$q$, anchor:"west")
      line(r,q, stroke: (dash: "dashed"))
      line(s,q, stroke: (dash: "dashed"))
    
      decorations.brace(r,p)
      decorations.brace(p,s)
      content((-0.7,0.7),$T_1$)
      content((-0.3,2.2),$T_2$)
    
      line((2.6,1.3),(3.1,2.1),mark:(start:">"))
      content((),[Spiegel],anchor:"west",padding:0.1)
    })]
    
    #underline[Behauptung:] $Delta s^2 = - c^2 T_1 T_2 < 0$, $Delta s^2:$ Intervall/Abstand zwischen $p$ und $q$
    
    #italic[Beweis:] Wähle Ruhesystem des Beobachters.
    #grid(
      columns: (1fr,1fr), align: center,
      canvas({
      import draw: *
      set-style(fill:black,radius:0.04,padding:0.15)
      let (r,p,s,q)=((0,0),(0,1),(0,3),(1.5,1.5))
      line((0,-1),(0,4),mark:(end:">"))
      content((),$x^0$,anchor:"west")
      line((-1.3,0),(4,0),mark:(end:">"))
      content((),$x^1$,anchor:"west")
      circle(r)
      content((),$r$, anchor:"north-east")
      circle(p)
      content((),$p$, anchor:"west")
      circle(s)
      content((),$s$, anchor:"south-west")
      circle(q)
      content((),$q$, anchor:"south",padding:0.25)
      circle((0,1.5))
      line((-1.5,1.5),q,stroke:(dash:"dashed"))
      line((-1.5,p.at(1)),(0,p.at(1)),stroke:(dash:"dashed"))
      line(r,q, stroke: (dash: "dashed"))
      line(s,q, stroke: (dash: "dashed"))
    
      decorations.brace(r,p)
      decorations.brace(p,s)
      decorations.brace((-1.5,p.at(1)),(-1.5,1.5))
      decorations.brace((q.at(1),0),r,)
      decorations.brace(q,(q.at(1),0),)
      content((-0.9,0.5),$c T_1$)
      content((-0.9,2),$c T_2$)
      content((-2.6,1.3),$c Delta T$)
      content((q.at(0)/2,-0.7),$Delta x$)
      content((q.at(0)*2.1,q.at(1)/2),$c/2(T_1+T_2)$)
    }),
      [
        #v(1cm)
        $
        Delta x = c/2 (T_1 + T_2), quad c Delta t = -c/2 (T_1 - T_2) \ 
        $
        $
        ==> Delta s^2 &= c^2 (Delta t)^2 - (Delta x)^2 \
        &= c^2 /4 (T_1 - T_2)^2 - c^2 /4 (T_1 + T_2)^2 \
        &= -c^2 T_1 T_2
        $
      ]
    )
  ]
)

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
    
    #bold[#underline[Zwillings-Paradoxon:]]
    #align(center)[#canvas({
      import draw: *
      let Koordinatensystem = {line((0,-0.5),(0,3),mark:(end:">"));content((),$x^0$,anchor:"east");line((-1,0),(3,0),mark:(end:">"));content((),$x^1$,anchor:"west")}
      set-style(fill:black,padding:0.1)
      Koordinatensystem
      line((0.5,3),(0.5,-0.5),stroke:(dash:"dashed"))
      content((),$A$,anchor:"north")
      line((1,3),(1,2.5))
      bezier((),(1.5,1.5),(1.5,2),fill:none)
      bezier((),(1,0.5),(1.5,1),fill:none)
      line((1,0.5),(1,-0.5))
      content((),$B$,anchor:"north")
      
      set-origin((7,0))
      Koordinatensystem
      line((0,0),(1,1.2))
      line((),(0,2.4))
      line((1,0),(1,1.2),stroke:(dash:"dashed"))
      content((0.5,-0.25),$Delta x$)
      decorations.brace((0,0),(0,1.2))
      decorations.brace((0,1.2),(0,2.4))
      content((-0.75,0.6),$T/2$)
      content((-0.75,1.8),$T/2$)
      content((1.55,0.8),$B$)
      line((1.35,0.8),(0.65,0.6),mark:(end:">"))
      bezier((1.5,1),(0.7,1.7),(1.55,2),mark:(end:">",fill:black),fill:none)
      content((0.65,2.8),$A$)
      line((0.5,2.55),(0.1,1.9),mark:(end:">"))
    })]
    
    Zeit von $A$: $T_A = T$. 
    
    Zeit von $B$: $c T_B = 2 sqrt(c^2 (T/2)^2 - Delta x^2)$
    $
    ==> T_B^2 = 4 (T^2 /4 - (Delta x^2) / c^2) = T_A^2 - 4 (Delta x^2)/c^2 \
    $
    #align(center, box(stroke: 0.5pt, inset: 0.5cm)[
    $
    ==> T_B < T_A
    $
    ])
    $=>$ Die Eigenzeit entlang von Geraden ist maximal.
  ]
)

== Lorentz-Transformation/Symmetrie von Minkowski

$x^mu --> x'^mu = tensor(Lambda, +mu, -nu) dot x^nu$

#bold[4-Vektoren:] $V^mu --> V'^mu = tensor(Lambda, +mu, -nu) dot V^nu$ ("kontravarianter Vektor")

#bold[co-Vektor:] $W_mu --> W'_mu = tensor((Lambda^(-1)), +nu, -mu) W_nu$ ("kovarianter Vektor")
$
V^mu W_mu -->  V'^mu W'_mu &= tensor(Lambda, +mu, -nu) V^nu tensor((Lambda^(-1)),+rho,-mu) W_rho \
&= tensor((Lambda^(-1)), +rho, -mu) tensor(Lambda, +mu, -nu) V^nu W_rho \
&= underbrace(tensor((Lambda^(-1) dot Lambda), +rho, -nu), tensor(delta, +rho, -nu)) dot V^nu W_delta \
&= V^nu W_nu
$

Analog für höhere Tensoren
$
T^(mu nu) &--> T'^(mu nu) = tensor(Lambda, +mu, -rho) tensor(Lambda, +nu, -sigma) T^(rho sigma) \
tensor(L, +mu, -nu) &--> tensor(L', +mu, -nu) = tensor(Lambda, +mu, -rho) tensor((Lambda^(-1)), +sigma, -nu) tensor(L, +rho, -sigma) "etc."
$

#bold[Lorentz-Gruppe:] 

Symmetrie von Minkowski = Invarianz von $eta_(mu nu)$
$
eta'_(mu nu) := tensor((Lambda^(-1)), +rho, -mu) tensor((Lambda^(-1)), +sigma, -nu) eta_(rho sigma) =^! eta_(mu nu) &<==> eta_(rho sigma) = tensor(Lambda, +mu, -rho) tensor(Lambda, +nu, -sigma) eta_(mu nu) \
&<==> eta = Lambda^T eta Lambda
$
$
==> det(eta) = det(Lambda^T) det(eta) det(Lambda) \
<==> 1 = det(Lambda)^2 <==> det(Lambda) = plus.minus 1
$
$
S O(1, 3) := { 1 in G L(4) | eta = Lambda^T eta Lambda, det(Lambda) = plus.minus 1 }
$

#bold[Beispiele:] $x^2 = x^3 = 0 quad x^mu --> x'^mu = (c t', x', 0, 0)$

Bestimme alle Transformationen, so dass $(c t')^2 - (x')^2 = (c t)^2 - x^2$

$-->$ Übungsaufgabe

#align(center)[#canvas({
  import draw: *
  set-style(radius:0.05,fill:black,padding:0.15,stroke:(dash:"dashed"))
  circle(())
  content((),$p$,anchor:"east")
  line((),(2,0))
  line((2,0),(2,1.5))
  content((),$q$,anchor:"west")
  circle(())
  content((1,0),$Delta x^1$,anchor:"north")
  content((2,0.75),$Delta x^0$,anchor:"west")
  set-origin((6,0))
  circle((0,0))
  content((),$p$,anchor:"east")
  line((),(2,0))
  circle(())
  content((),$q$,anchor:"west")
  content((1.75,1.5),[Welche Transformationen sind möglich?])
  set-origin((7,0))
  circle((0,-0.25))
  content((),$p$,anchor:"east")
  line((),(0,1.25))
  circle(())
  content((),$q$,anchor:"east")
})]

#bold[Physikalische Freiheitsgrade in SR]

1. Punktteilchen (elektrische Ladungen, ...)
2. Felder (elektrisches/magnetisches Feld)

#align(center, italic[Abbildung])

$
Delta s_i = sqrt(eta_(mu nu) Delta x_i^mu Delta x_i^nu) \
s(p, q) = sum_i Delta s_i = sum_i sqrt(eta_(mu nu) Delta x_i^mu Delta x_i^nu) \
$
$
s(p, q) = integral_C dd(s) := integral_a^b sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) dd(lambda)
$
Zeitartig: $dot(x)^2 = eta_(mu nu) dot(x)^mu dot(x)^nu > 0$

#bold[Eigenzeit:] $T = 1/c integral dd(s) = 1/c integral dd(lambda) sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu,lambda))$

Bewegungsgleichung für freies Teilchen?

$-->$ Wirkung
$
S[x(lambda)] = -m c integral dd(s) = - m c integral sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu,lambda)) dd(lambda)
$
klassische Mechanik: $S = integral dd(t) 1/2 m dot(q)^2 plus.minus ...$ und $[S] = "Zeit" dot "Energie"$

== Teilchen in SR

#align(center, italic[Abbildung])

Parametrisierung: $x^nu (lambda), lambda in [a, b]$ mit $x(a) = p$ und $x(b) = q$.

Tangentialvektor: $dot(x)^nu (lambda) := dv(x^nu, lambda)$, zeitartig: $dot(x)^2 := eta_(mu nu) dot(x)^mu dot(x)^nu > 0$

#bold[Raumzeit-Distanz/Intervall der Kurve $C$:]
$
integral_C dd(s) := integral_a^b sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) dd(lambda) = integral_a^b sqrt(dot(x)^2) dd(lambda)
$
$==>$ #bold[Eigenzeit] $tau = s/c$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  Delta tau := 1/c integral sqrt(dot(x)^2) dd(lambda)
  $
])
Zeit, gemessen von einer Uhr mit Weltlinie $C$.

#line(length: 100%)

#bold[Wirkung (Hamiltonisches Prinzip)] für ein freies Teilchen:
$
S = -m c integral sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) dd(lambda) = - m c^2 integral dd(tau)
$

#bold[Variation:]
#grid(
  columns: (2fr,1fr),align:center,
  [
    #v(0.8cm)
  #box(stroke: 0.5pt, inset: 0.5cm)[
    $
    delta S := eval(dv(,epsilon) S[x(t) + epsilon dot delta x(t)])_(epsilon = 0)=^!0
    $
  ]],
  canvas({
    import draw: *
    set-style(radius:0.05,padding:0.15)
    circle((),fill:black)
    content((),$x(a)$,anchor:"north")
    bezier((0,0),(0.5,1),(0,0.5))
    bezier((0.5,1),(0.8,1.9),(0.95,1.4))
    bezier((0.8,1.9),(1.05,2.7),(0.7,2.4))
    circle((),fill:black)
    content((),$x(b)$,anchor:"south-west",padding:0.1)
    set-style(stroke:(dash:"dashed"))
    bezier((0,0),(0.5,1),(-0.3,0.8))
    bezier((0.5,1),(0.8,1.9),(1.15,1.15))
    bezier((0.8,1.9),(1.05,2.7),(0.3,2.5))
    content((3,1),$S[x(lambda)]in RR$)
  })
)

Rechnung: Variation des Funktionals

1) $delta$ und $dv(,lambda)$ kommutieren
$
delta (dv(x^mu, lambda)) = eval(dv(,epsilon) dv((x^mu+epsilon delta x^mu),lambda))_(epsilon = 0) = eval(dv((delta x^mu), lambda))_(epsilon=0) = dv(,lambda) (delta x^mu)
$
2) 
$
delta (eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) =^("1)") eta_(mu nu) dv((delta x^mu), lambda) dv(x^nu, lambda) + eta_(mu nu) dv(x^mu, lambda) dv((delta x^nu), lambda) = 2 eta_(mu nu) dv((delta x^mu), lambda) dv(x^nu, lambda)
$
3)
$
delta sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) &=^"Kettenregel" 1/(2 sqrt(dot(x)^2)) delta (dot(x)^2) #h(2cm) "wobei" dot(x)^2 equiv eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda) \
&=^("2)") 1/sqrt(dot(x)^2) dv(,lambda) (delta x^mu) dv(x^mu, lambda) \
==> delta S = - m c integral delta& sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) dd(lambda) = - m c integral dv(,lambda) (delta x_mu) 1/sqrt(dot(x)^2) dv(x^mu, lambda) dd(lambda) =^! 0
$
Partielle Integration davon + Annahme: $eval(delta x_mu)_a = eval(delta x_mu)_b = 0$
$
delta S = m c integral delta x_mu dv(,lambda) (1/sqrt(dot(x)^2) dv(x^mu, lambda)) dd(lambda) =^!0 quad forall delta x^mu
$

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> dv(,lambda) (1/sqrt(dot(x)^2) dv(x^mu, lambda)) = 0
  $
])

#bold[Euler-Lagrange-Gleichung:]
#grid(
  columns: (6fr,1fr,6fr), align: center,
  box(stroke: 0.5pt, inset: 0.5cm)[
    $
    dv(,lambda) (c/sqrt(dot(x)^2) dv(x^mu, lambda)) = 0
    $
  ],
  [
    #v(0.7cm)
    $<==>$
  ],
  box(stroke: 0.5pt, inset: 0.5cm)[
    $
    dv(u^mu, lambda) = 0, space u^mu := c/sqrt(dot(x)^2) dv(x^mu, lambda)
    $
  ]
)

$u^mu$: 4er-Geschwindigkeit
$
u^2 = u^mu u_mu = c^2/dot(x)^2 underbrace(dot(x)^mu dot(x)_mu, dot(x)^2) = c^2
$
Wähle zwei Parametrisierungen:

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
#bold[1) Eigenzeit:] Wähle als Parametrisierung der Kurve die Eigenzeit

#grid(
  columns: (2fr,1fr), align: center,
  [
    #v(0.7cm)
$
tau(lambda) := 1/c integral_(lambda_0)^lambda sqrt(eta_(mu nu) dv(x^mu, lambda) dv(x^nu, lambda)) dd(lambda)
$],
canvas({
  import draw: *
  set-style(radius:0.04,padding:0.1)
  bezier((-0.5,-1.2),(0,-0.6),(-0.15,-1),stroke:(dash:"dashed"))
  circle((),fill:black)
  content((),$x^mu (lambda_0)$,anchor:"north-west")
  bezier((0,-0.6),(0.1,0.6),(0.25,0),stroke:(thickness:1.6pt))
  circle((),fill:black)
  content((),$x^mu (lambda)$,anchor:"south-west")
  bezier((0.1,0.6),(0.1,1.5),(-0.15,1),stroke:(dash:"dashed"))
})
)
Bewegungsgleichungen generell:
$ dv(u^mu,tau)=0 $
Betrachte $u^mu$:
#grid(
  columns: (2fr,1fr), align: center,
  $ => u^mu=c/sqrt(dot(x)^2) dv(x^mu,lambda)limits(=)_limits(arrow.t)_"Kettenregel"c/sqrt(dot(x)^2) dv(x^mu,tau) underbrace(dv(tau,lambda),1/c sqrt(dot(x)^2))=dv(x^mu,tau) $,
  box(stroke: 0.5pt, inset: 0.2cm)[
    $
    ==> u^mu = dv(x^mu, tau)
    $
  ]
)
$==>$ Bewegungsgleichung wird zu:
#grid(
  columns: (1fr,1fr), align: center,
  [
    #v(0.7cm)
    $ 0 = dv(u^mu, tau) = dv(x^mu, tau, 2) $
  ],
  canvas({
    import draw: *
    line((-0.5,0),(2,0),mark:(end:">",fill:black))
    line((0,-0.5),(0,2),mark:(end:">",fill:black))
    circle((0.5,0.4),radius:0.05,fill:black)
    content((),$x_0^mu$,anchor:"west",padding:0.2)
    line((),(0.9,1.6),mark:(end:">"))
    content((),$u_0^mu$,anchor:"north-west",padding:0.1)
    line((),(1.09,2.15),stroke:(dash:"dashed"))
  })
)
Die Bewegung eines freien Teilchens wird also durch eine Gerade beschrieben:
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> x^mu (tau) = u_0^mu dot tau + x_0^mu quad u_0^mu, x_0^mu = "const."
  $
])
$
u^mu u_mu = u_0^mu u_(0 mu) = c^2 > 0 ==> "zeitartig"
$
]
)

#block(
  fill: luma(230),
  inset: 8pt,
  radius: 4pt,
  width: 100%,
  [
#bold[2) Koordinatenzeit:] Wähle als Parametrisierung der Kurve $lambda=t$

$ x^0 = c t,& quad "Setze" lambda = t ==> x^mu (t) = (c t, x^i (t)) $

#grid(
  columns: (1fr,1fr), align: center,
  [ 
    $
    #h(1cm)&==> dot(x)^mu =  (c, dv(x^i, t)) equiv (c, v^i) \ 
    &==> dot(x)^2 = c^2 - abs(va(v))^2 \
    &==> 1/sqrt(dot(x)^2) = underbrace(1/sqrt(1 - v^2/c^2),:=gamma) dot 1/c = 1/c dot gamma \ 
    &"Damit:" u^mu = c/sqrt(dot(x)^2) dot(x)^mu = gamma dot(x)^mu = gamma(c, va(v))
    $
  ],
  canvas({
    import draw: *
    line((-1,0,0),(3,0,0),mark:(end:">",fill:black))
    content((),$x^1$,anchor:"west",padding:0.15)
    line((0,-1,0),(0,3,0),mark:(end:">",fill:black))
    content((),$x^0=c t$,anchor:"south",padding:0.15)
    line((0,0),(2.5,2.5))
    line((0,0),(-1,1))
    set-style(stroke:(dash:"dashed"))
    bezier((0,0),(0.5,1),(0.1,0.5))
    bezier((0.5,1),(0.8,1.9),(0.95,1.4))
    bezier((0.8,1.9),(1.05,2.7),(0.7,2.4))
  })
)

$
S = - m c integral dd(s) = - m c integral dd(t) sqrt(dot(x)^2) = - m c^2 integral dd(t) sqrt(1- va(v)^2/c^2)
$
Lagrange-Funktion: 
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> L = -m c^2 sqrt(1 - va(v)^2/c^2)
  $
])

Energie/#bold[Hamiltonische Funktion:] $p_i := pdv(L, dot(x)^i) = gamma dot m dot(x)_i$

$
==> E=H := p_i dot(x)^2 - L = ... = gamma dot m c^2 approx underbrace(m c^2,"Ruheenergie")+p^2/(2 m)+cal(O)(p^4)
$
$==>$ Ruheenergie
#align(center, box(stroke: 0.5pt, inset: 0.25cm)[
  $
  E = m c^2
  $
])

#bold[Ladungsdichte $rho$ und Strom $va(j)$]

Stationärer Fall: $x^mu (lambda) =^(lambda = t) x^mu (t) = (c t, va(x)_0)$

Geladenes Teilchen mit Ladung $e$ am Ort $va(x)_0$.
$
rho(va(x)_0)= e dot delta^3 (va(x) - va(x)_0), quad va(j)(va(x)) = 0
$
])
#bold[Wiederholung: Dirac "$delta$-Funktion"]

#align(center)[#canvas({
  import draw: *
  let epsilon=calc.pow(10,-2)
  plot.plot(
    axis-style: none, // ich weiß nicht, wie ich die Farbe ändere lol
    plot.add(
      domain:(-1,1),
      x=>1/calc.pi*epsilon/(epsilon*epsilon+x*x)
    )
  )
  content((1.1,1),$delta(x)$)
})]

$
integral_(-oo)^oo dd(x) delta(x) = 1 wide integral_(-oo)^oo dd(x) delta(x - a) f(x) = f(a)
$

Keine Funktion $delta(x)$ hat exakt diese Eigenschaften, aber wir können sie beliebig genau annähern.
$
Delta_epsilon (x) := 1/pi epsilon/(x^2 + epsilon^2), space epsilon > 0 wide \" delta(x) = lim_(epsilon -> 0) Delta_epsilon (x) \"
$

#bold[Eigenschaften der $delta$-Funktion:]

#boxedlist[
  $f(x) delta(x) = f(0) delta(x)$
][
  Für glatte Funktion $f(x)$ mit Nullstellen $x_n$, so dass $f'(x_n) != 0$
  $
  &==>^"ÜA" delta(f(x)) = sum_n 1/abs(f'(x_n)) delta(x - x_n) \ 
  &==> "Spezialfall:" delta(a dot x) = 1/abs(a) delta(x) quad "für" a in RR 
  $
]

Dirac $delta$-Funktion in höheren Dimensionen:
$
delta^3 (va(x) - va(y)) = delta(x^1 - y^1) delta(x^2 - y^2) delta(x^3 - y^3) "etc."
$
z.B.:
$
integral_V dd(va(y), [3,]) f(va(y)) delta^3 (va(x) - va(y)) = f(va(x)) \
==> integral dd(va(x), [3,]) rho(va(x)) = e integral dd(va(x), [3,]) delta^3 (va(x) - va(x)_0) = e ==> Q = e "Gesamtladung"
$

#bold[Kontinuitätsgleichung:] Für zeitabhängige Ladungsdichte $rho(va(x), t)$ hat man eine Relation zum Strom $va(j)$ 

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  pdv(rho, t) + div va(j) = 0
  $
])

#bold[Interpretation:] Für zeitabhängige $rho(va(x), t)$ ist die Gesamtladung in $V$ zeitabhängig.

#align(center, italic[Abbildung])

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  Q_v (t) := integral_V dd(va(x), [3,]) rho(va(x), t)
  $
])
$
dv(Q_v (t), t) = integral_V dd(va(x), [3,]) pdv(rho(va(x), t), t) = - integral dd(va(x), [3,]) div va(j) =^"Gauß" - integral_(diff V) va(j) dot va(dd(Sigma)) \ = "Gesamtstrom durch die Oberfläche"
$
Für #bold[relativistische] Teilchen: $rho$ und $va(j)$ kombinieren sich zu relativistisch kovarianten 4-Vektoren.
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  j^mu := (c rho, va(j))
  $
])
$j^0 = c rho, j^i = (va(j))^i$

Eine Erhaltungsrelation:
$
diff_mu j^mu = diff_0 j^0 + sum_(i=1)^3 diff_i j^i = pdv(,x^0) (c rho) + div va(j) = pdv(, t) rho + div va(j) = 0
$

#bold[4er-Strom für geladenes Punktteilchen?]

Weltlinie des Teilchens mit Ladung $e$: $x^mu (lambda)$

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  j^mu (x) = c e integral dd(lambda) dv(x^mu, lambda) delta^4 (x- x(lambda))
  $
])
Beachte: $x corres$ Punkt im $RR^(3, 1)$ aber $x(lambda) corres$ 4 Funktionen der Parametrisierung 

Wähle Parametrisierung nach Koordinatenzeit: $x^mu (lambda) = x^mu (t') = (c t', va(x)(t'))$

$
j^0 (c t, va(x)) &= c e integral dd(t') dv(x^0 (t'), t') underbrace(delta(c t - c t'), 1/c delta(t-t')) delta^3 (va(x) - va(x)(t')) \
&= c e integral dd(t') delta(t - t') delta^3 (va(x) - va(x)(t')) \ 
&= c e delta^3 (va(x) - va(x)(t)) \
&= c dot rho(va(x), t)
$

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> rho(va(x), t) = e delta^3 (va(x) - va(x)(t)) 
  $
])

Konsistent mit dem stationären Fall

#bold[3er-Strom:] 
$
j^i (c t, va(x)) = c e integral dd(t') underbrace(dv(x^i (t'), t'), = v^i (t')) underbrace(delta(c(t-t')), =1/c delta(t -t')) integral^3 (va(x) - va(x)(t')) \
= e integral dd(t') v^i (t') delta(t - t') delta^3 (va(x) - va(x)(t')) = e v^i (t) delta^3 (va(x) - va(x)(t))
$

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  &j^0 = c rho = c r delta^3 (va(x) - va(x)(t)) \
  &va(j) = e va(v)(t) delta^3 (va(x) - va(x)(t))
  $
])

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  j^mu (x) := c e integral dd(lambda) dv(x^mu, lambda) delta^4 (x - x(lambda))
  $
])

#bold[Behauptung:] $diff_mu j^mu = pdv(j^mu, x^mu) = 0$

Beweis: Test-Funktion $phi(x)$ (Skalar im $RR^3$)
#align(center, italic[Abbildung])
$
integral dd(x, [4,]) phi(x) diff_mu j^mu (x) &=^"Def." c e integral dd(lambda) dv(x^mu (lambda), lambda) integral dd(x, [4,]) diff_mu delta^4 (x - x(lambda)) \
&=^"part. Int." eval(- c e integral dd(lambda) dv(x^mu, lambda) integral dd(x, [4,]) diff_mu phi(x) delta^4 (x - x(lambda)))_(x^mu = x^mu (lambda)) \
&= eval(- c e integral dd(lambda) dv(x^mu, lambda) pdv(phi(x), x^mu))_(x = x(lambda)) \
&= - c e integral dd(lambda) dv(,lambda) phi(x(lambda)) = 0
$
Für beliebige Testfunktionen

#line(length: 100%)

#bold[Resultate bisher:]

#boxedlist[
  #bold[Ladungsdichte] einer Punktladung $e$ mit Bahnkurve $va(x)(t)$
  $
  rho(t, va(x)) = e delta^3 (va(x) - va(x)(t))
  $
][
  #bold[Stromdichte:] 
  $
  va(j)(t, va(x)) = e va(v)(t) delta^3 (va(x) - va(x)(t)), quad va(v)(t) = dv(va(x),t)
  $
  Relativistisch kovariante Form $j^mu (x) = (c rho(x), va(j)(x))$
]

Limes vieler Punktladungen:

Glatte Funktion $rho(x)$, glattes Vektorfeld $va(j)(x)$

#v(0.2cm)
#align(center)[#canvas({
  import draw: *
  bezier((0,0),(0.5,1),(0.5,0.5))
  bezier((),(1,1.5),(0.45,1.5))
  bezier((),(1.9,0),(2,1.5))
  bezier((),(0.8,-0.5),(1.8,-0.6))
  bezier((),(0.2,-0.46),(0.5,-0.43))
  bezier((),(0,0),(-0.25,-0.4))

  set-style(mark:(end:"stealth",fill:black))
  line((0.35,-0.1),(rel:(0.5,0.4)))
  line((0.7,0.8),(rel:(0.3,0.5)))
  line((1.15,0.7),(rel:(0.4,0.4)))
  line((1,-0.2),(rel:(0.6,0.1)))
  line((1.3,0.2),(rel:(0.4,0.4)))

  set-origin((6.5,1.5))
  rotate(z:-70deg)
  set-style(mark:(end:none))
  bezier((0,0),(0.5,1),(0.5,0.5))
  bezier((),(1,1.5),(0.45,1.5))
  bezier((),(1.9,0),(2,1.5))
  bezier((),(0.8,-0.5),(1.8,-0.6))
  bezier((),(0.2,-0.46),(0.5,-0.43))
  bezier((),(0,0),(-0.25,-0.4))
  
  set-style(mark:(end:"stealth"))
  line((0.45,-0.1),(rel:(0.3,0.6)))
  line((0.7,0.8),(rel:(0.3,0.5)))
  line((1.15,0.7),(rel:(0.4,0.4)))
  line((1.3,-0.2),(rel:(0.5,0.3)))

  rotate(z:70deg)
  set-style(mark:(end:none))
  line((-5.2,-1.3),(0.4,-1))
  circle((),radius:0.05,fill:black)
  line((),(rel:(0.6,-0.1)),mark:(end:">"))
  circle((-5.2,-1.3),radius:0.05,fill:black)
  content((-2.5,-0.8),$r$)
  content((-2.5,0.25),align(center)[Coulomb Potential \ $phi.alt(r)tilde 1/r$])
})]

$-->$ instantane Wechselwirkung (Kraft) nicht kompatibel mit SR!

$-->$ Feldtheorie, dynamische Felder $va(E)(t, va(x)), va(B)(t, va(x))$

== Relativistische Feldtheorie

Eine Feldtheorie besteht aus dynamischen Felder, die jeden Punkt des Raums $RR^3$ (oder der Raumzeit $RR^(3, 1)$) eine Zahl, Vektor, Matrix etc. zuweisen.

#bold[Beispiel:] Skalarfeld: $phi(t, va(x))$, Vektorfeld: $va(A)(t, va(x))$

#bold[Unterschied zu Punktteilchen:]

$-->$ Bewegungsgleichungen sind #underline[gewöhnliche] Differentialgleichungen:
#grid(
  columns: (1fr,1fr), align: center,
  [
    #v(0.7cm)
$
m dv(va(x), t, 2) = va(F)(va(x)(t))
$
 ],
 canvas({
   import draw: *
   content((1,3.1),$x(t)in RR^3$)
   bezier((0.5,1),(0.8,1.9),(0.95,1.4),mark:(end:">"))
   bezier((0.8,1.9),(1.05,2.7),(0.7,2.4))
 })
)
In Feldtheorie: $-->$ #underline[partielle] Differentialgleichungen für $phi(t, va(x))$, $va(A)(t, va(x))$

#bold[Beispiel:] Kontinuitätsgleichung für $rho(t, va(x))$, $va(j)(t, va(x))$
$
pdv(rho, t) + div va(j) = 0
$

#bold[Felder auf Minkowski-Raum $RR^(3, 1)$:]
$
x^mu = (c t, va(x)), quad mu = 0,1,2,3, quad eta_(mu nu) = diagonalmatrix(1,-1,-1,-1)
$
#bold[Skalarfeld:] $phi = phi(x) = phi(x^mu) = phi(x^0, x^1, x^2, x^3)$

Lorentz-Transformation: $x^mu -> x'^mu = tensor(Lambda, +mu, -nu) x^nu quad [x' = Lambda dot x]$
$
phi --> phi' wide "so dass" wide phi'(x') = phi(x) \
<==> phi'(x') = phi'(Lambda x) = phi(x) quad "für alle" x in RR^3
$
$y := Lambda dot x, x = Lambda^(-1) y$
$
==> phi'(y) = phi(Lambda^(-1) y) quad "für alle" y in RR^(3,1)
$
Umbennen: $y -> x$:
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  phi'(x) = phi(Lambda^(-1) x)
  $
])

#bold[Vektorfelder:] $A^mu = A^mu (x)$, $A^mu -> A'^mu$, wobei
$
A'^mu (x') = tensor(Lambda, +mu, -nu) A^nu (x) <==>
$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  A'^mu (x) = tensor(Lambda, +mu, -nu) A^nu (Lambda^(-1) x)
  $
])

#bold[Höhere Tensoren:] $F^(mu nu) (x) = - F^(nu mu) (x)$
$
F'^(mu nu) (x) = tensor(Lambda, +mu, -rho) tensor(Lambda, +nu, -sigma) F^(rho sigma) (Lambda^(-1) x)
$

#bold[Partielle Ableitungen/Differentialgleichungen:]
$
diff_mu := pdv(,x^mu)
$
Unter Lorentz-Transformationen: $x^mu -> x'^mu = tensor(Lambda, +mu, -nu) x^nu$
$
diff_mu &= pdv(,x^mu) = pdv(x'^nu, x^mu) pdv(,x'^nu) \ 
&= tensor(Lambda, +nu, -mu) pdv(,x'^nu) = tensor(Lambda, +nu, -mu) diff'_v
$
$
==> diff'_nu = tensor((Lambda^(-1)), +mu, -nu) diff_mu <==>
$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  diff'_mu = tensor((Lambda^(-1)), +nu, -mu) diff_nu
  $
])

#bold[Beispiel:] 
$
square := diff^mu diff_mu = eta^(mu nu) diff_mu diff_nu = eta^(mu nu) pdv(,x^mu,x^nu,[1,1]) = pdv(,(x^0), [2,]) - pdv(,(x^1), [2,]) - pdv(,(x^2), [2,]) - pdv(,(x^0), [3,]) = 1/c^2 pdv(,t,[2,]) - laplace
$

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  Laplace-Operator:
  $
  laplace := div grad
  $
])

#bold[Wellengleichungen:]

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  Laplace-Operator:
  $
  (1/c^2 pdv(,t,[2,]) - laplace) phi(t, va(x)) = 0
  $
])

== Maxwell-Gleichungen

#bold[Empirischer Input:]

#boxedenum[
  Es existieren elektrische Felder $va(E)(t, va(x))$ und magnetische Felder $va(B)(t, va(x))$ 
][
  $va(B) <-->$ bewegte Ladungen ($va(j)$) für stationäre Punktladungen am Ort $va(x)_0$:
  $
  va(E)(va(x)) = e/(4 pi epsilon_0) (va(x) - va(x)_0)/abs(va(x) - va(x)_0)^3
  $

  #bold[Übungsaufgabe:] $div va(E) = 0$ für $va(x) != va(x)_0$, $rho$: Ladungsdichte
  $
  div va(E) = 1/epsilon_0 rho
  $
][
  Es existieren #bold[keine] magnetischen Ladungen
  $
  0 = integral_(diff V) va(B) dot dd(va(Sigma)) = integral_V dd(x, [3,]) div va(B) = 0 quad forall V \
  $
  #align(center, box(stroke: 0.5pt, inset: 0.5cm)[
    $
    ==> div va(B) = 0
    $
  ])
]

Welches Feld im Minkowski-Raum beinhaltet $va(E)$ und $va(B)$?

Antisymmetrisch: $F_(mu nu) = - F_(nu mu)$
$
mat(0,F_(0 1),F_(0 2),F_(0 3);-F_(0 1),0,F_(1 2),F_(1 3);-F_(0 2),-F_(1 2),0,F_(2 3);-F_(0 3),-F_(1 3),-F_(2 3),0)
$

$j^mu = (c rho, va(j)), va(E) op(tilde) c rho = j^0$

Da die erste Komponente vom Viererstrom $j^0=c rho$, schreiben wir $vb(E)$ in die $F_(0j)$ Komponenten. $vb(B)$ schreiben wir in die $F_(i j)$ Komponenten, da es mit $j^i=arrow(j)$ assoziiert wird.

#bold[Feldstärke-Tensor:]
$
F_(mu nu) = mat(0,E^1,E^2,E^3;-E^1,0,-B_3,B_2;-E^2,B_3,0,-B_1;-E^3,-B_2,B_1,0)
$

Effizientere Notation:
$
F_(0 i) = E_i, quad i = 1,2,3 \
F_(i j) = - epsilon_(i j k) B^k
$
#line(length: 100%)
$
&F^(mu nu) := eta^(mu rho) eta^(nu sigma) F_(rho sigma) \
==> &F^(0 i) = eta^(0 0) eta^(i j) F_(0 j) = - delta^(i j) E_j = - E^i \
&F^(i j) = eta^(i k) eta^(j l) F_(k l) = - epsilon^(i j k) B_k
$
$
==> F^(mu nu) = mat(0,-E^1,-E^2,-E^3;E^1,0,-B_3,B_2;E^2,B_3,0,-B_1;E^3,-B_2,B_1,0)
$

#bold[Maxwell-Gleichungen:] 

Empirische Annahmen: (Gauss: $epsilon_0 -> 1/(4 pi)$)

#grid(columns: (1fr,1fr), align: center, 
  [ 
    #v(0.35cm)
    #box(stroke: 0.5pt, inset: 0.5cm)[
    $
    div va(E) &= 4 pi rho \
    div va(B) &= 0
    $
  ]],
  canvas({
    import draw: *
    circle((),radius:0.7,name:"circle",stroke:(dash:"dashed"))
    line(("circle"),(1.3,-0.5),mark:(end:">",fill:black))
    content((),$arrow(E)$,anchor:"south",padding:0.27)
    line(("circle"),(-1.3,-0.5),mark:(end:">",fill:black))
    line(("circle"),(-1.3,0.5),mark:(end:">",fill:black))
    line(("circle"),(0,1.3),mark:(end:">",fill:black))
    line(("circle"),(0.4,-1.3),mark:(end:">",fill:black))
    circle((0,0),radius:0.05,fill:black,stroke:none)
    content((),$q$,anchor:"west",padding:0.1)
    set-origin((4.5,0))
    circle((0,0),radius:0.7,name:"circle",stroke:(dash:"dashed"))
    let (a,b,c)=((-0.7,-1.1),(-0.7,1),(0.3,-0.3))
    bezier(a,b,c)
    bezier(a,b,c,mark:(end:">",fill:black, pos:25%))
    bezier(a,b,c,mark:(end:">",fill:black, pos:75%))
    bezier((a.at(0)+0.7,a.at(1)-0.1),(b.at(0)+0.6,b.at(1)),(c.at(0)+0.5,c.at(1)))
    content((),$arrow(B)$,anchor:"south-west",padding:0.1)
    bezier((a.at(0)+0.7,a.at(1)-0.1),(b.at(0)+0.6,b.at(1)),(c.at(0)+0.5,c.at(1)),mark:(end:">",fill:black, pos:25%))
    bezier((a.at(0)+0.7,a.at(1)-0.1),(b.at(0)+0.6,b.at(1)),(c.at(0)+0.5,c.at(1)),mark:(end:">",fill:black, pos:80%))
  })
)
Motivation dessen:
$
integral_(diff V) va(E) dot dd(va(Sigma)) = 4 pi Q wide wide 0 = integral_(diff V) va(B) dot dd(va(Sigma)) = integral_V dd(x,[3,]) div va(B)
$
#line(length: 100%)
$
div va(E) = diff_i E^i = - diff_i F^(0 i) = diff_i F^(i 0) = 4 pi rho = (4 pi)/c j^0 \
==> diff_i F^(i o) = (4 pi)/c j^0
$
#bold[Postulat (inhomogene Maxwell Gleichungen):]
#align(center, box(stroke: 1pt, inset: 0.5cm)[
  $
  diff_mu F^(mu nu) = (4 pi)/c j^nu
  $
])

$
div va(B) = diff_i B^i = 0 wide F_(i j) = - epsilon_(i j k) B^k \
==> epsilon^(i j l) F_(i j) = i epsilon^(i j l) epsilon_(i j k) B^k = -2 B^l \
$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> B^i = -1/2 epsilon^(i j k) F_(j k) = 0
  $
])
$
div va(B) = diff_i B^i = -1/2 epsilon^(i j k) diff_i F_(j k) = 0
$

#bold[4D Levi-Civita-Tensor:] $epsilon^(mu nu rho sigma)$: total antisymmetrisch, $epsilon^(0123) = +1$, Lorentz-invariant unter $S O(1, 3)$
$
==> epsilon^(0 i j k) diff_i F_(j k) = 0
$

#bold[Postulat (homogene Maxwell Gleichungen):]
#align(center, box(stroke: 1pt, inset: 0.5cm)[
  $
  epsilon^(mu nu rho sigma) diff_nu F_(rho sigma) = 0
  $
])
$
<==>
$
#align(center, box(stroke: 1pt, inset: 0.5cm)[
  $
  diff_mu F_(nu rho) + diff_nu F_(rho mu) + diff_rho F_(mu nu) = 0
  $
])
Es bleibt die Konsequenzen dieser Theorie auszurechnen und auf Konsistenz zu prüfen.

Betrachte die inhomogenen Maxwellgleichungen.

$nu = 0$:
$
diff_mu F^(mu 0) &= underbrace(diff_0 F^(0 0), = 0) + diff_i F^(i 0) = diff_i E^i = div va(E) \
&= (4 pi)/c j^0 = (4 pi)/c c rho = 4 pi rho \
&==> div va(E) = 4 pi rho
$
$nu = j$:
$
diff_mu F^(mu j) &= diff_0 F^(0 j) + diff_i F^(i j) \
&= diff_0 (- E^j) + diff_i (- epsilon^(i j k) B_k) \
&= - pdv(E^j, x^0) + epsilon^(j i k) diff_i B_k \ 
&=^(x^0 = c t) - 1/c pdv(E^j, t) + (curl va(B))^j \
&=^! (4 pi)/c j^j 
$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  ==> - 1/c pdv(va(E), t) + curl va(B) = (4 pi)/c va(j)
  $
])

#underline[Divergenz:]
$
- 1/c pdv(,t) underbrace((div va(E)), = 4 pi rho) + underbrace(div(curl va(B)), = 0) = (4 pi)/c va(j) \
==> - pdv(, t) rho = div va(j) ==> pdv(rho,t) + div va(j) = 0 \
"Kontinuitätsgleichung"
$

Betrachte die homogene Maxwellgleichungen:

$mu = 0$: 
$
wide epsilon^(0 i j k) diff_i F_(j k) = 0 quad <==> quad diff_i B^i = div va(B) = 0
$

$mu = i$:
$
epsilon^(i 0 j k) diff_0 F_(j k) + epsilon^(i j 0 k) diff_j F_(0 k) + epsilon^(i j k 0) diff_j F_(k 0) = 0 \
<==>
$
#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  1/c pdv(va(B), t) + curl va(E) = 0
  $
])

#pagebreak()

#align(center, text(style: "oblique", size: 16pt)[Maxwell-Gleichungen])

#align(center, stack(dir: ltr, 
  box(width: 5cm, height: 3.5cm, stroke: 0.5pt, inset: 0.5cm)[
    #bold[inhomogen:]
    $
    div va(E) &= 4 pi rho \
    curl va(B) &= 1/c pdv(va(E),t) + (4 pi)/c va(j)
    $
  ], 
  box(width: 5cm, height: 3.5cm, stroke: 0.5pt, inset: 0.5cm)[
    #bold[homogen:]
    $
      div va(B) &= 0 \
      curl va(E) &= - 1/c pdv(va(B), t)
    $
]))

Im Vakuum ($rho = 0 = va(j)$), #bold[elektrisch-magnetische Dualität]: invariant unter

#align(center, box(stroke: 0.5pt, inset: 0.5cm)[
  $
  va(E) --> va(B), quad va(B) --> -va(E)
  $
])

#bold[Kommentar:] Andere Konventionen sind oft üblich
$
F^(mu nu) = mat(0,-1/c E_x,-1/c E_y,-1/c E_z;1/c E_x,0,-B_z,B_y;1/c E_y,B_z,0,-B_x;1/c E_z,-B_y,B_x,0)
$
SI Einheiten: $epsilon_0 dot mu_0 = 1/c^2$, $epsilon_0$: Permittitivität (des leeren Raumes), $mu_0$: Permeabilität (des leeren Raumes)

nach obigem $quad va(E) -> 1/c va(E)$, $quad epsilon -> 1/(4 pi c)$

Maxwell-Gleichungen in SI Einheiten:
#align(center, stack(dir: ltr, 
  box(width: 5.5cm, height: 3.9cm, stroke: 0.5pt, inset: 0.5cm)[
    #bold[inhomogen:]
    $
    div va(E) &= rho/epsilon_0 \
    curl va(B) &= mu_0 va(j) + 1/c^2 pdv(va(E),t)
    $
  ], 
  box(width: 5cm, height: 3.9cm, stroke: 0.5pt, inset: 0.5cm)[
    #bold[homogen:]
    $
      div va(B) &= 0 \ \
      curl va(E) &= - pdv(va(B),t)
    $
]))
