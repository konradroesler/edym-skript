#import "@preview/physica:0.9.3": *

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

comment(Zeichung 2D system mit massepunkt, e eingezeichnet)

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

comment(Zeichung zweier Punktteilchen, Coloumbgesetz geometrisch

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
comment(Zeichung Punktteilchen, Ladung)

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

/* #let grad = $"grad"$ // omg
#let rot = $"rot"$
#let div = $"div"$ */

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
d := diff_i dd(x^i) and
$
Beispiel: 
$
d A &= d (A_j d x^j) \
&= diff_i d x^i and (A_j d x^j) \
&= diff_i A_j d x^i and d x^j \ 
&= 1/2 (diff_i A_j - diff_j A_i) underbrace(d x^i and d x^j, - d x^j and  d x^i)
$

$Omega^p:$ $p$-Formen, $d: Omega^p --> Omega^(p+1)$, $d^2 = 0$ (Übungsaufgabe)

#underline[Hodge Operator:]
$
star: Omega^p <--> Omega^(3-p) \
star: Omega^1 <--> Omega^(2) \
star: Omega^3 <--> Omega^(0) \
$
$A$ ist 1-Form, $B$ ist 2-Form, $C$ ist 3-Form
$
star A &= 1/2 epsilon_(i j)^k A_k d x^i and d x^j \
star B &= 1/2 epsilon_i^(j k) B_(j k) d x^i \
star C &= 1/3! epsilon^(i j k) C_(i j k)
$
Wir erweitern das Diagramm von vorher:
$
&"Skalare" -->^"grad" "Vektoren" -->^"rot" "Vektoren" -->^"div" "Skalare" \
&arrow.b.t i d #h(1.6cm) arrow.b.t ♡ #h(1.9cm) arrow.b.t star, ♡ #h(1.5cm) arrow.b.t star\
&Omega^0 #h(1cm) -->^d Omega^1 #h(1.35cm) -->^d Omega^2 #h(1.35cm) -->^d Omega^3
$
Dieses Diagramm kommutiert. (Alle Pfade, die zwei Punkte verbinden, sind  äquivalent.)
$
d^2 = 0 <==> "rot" compose "grad" = 0, "div" compose "rot" = 0
$
