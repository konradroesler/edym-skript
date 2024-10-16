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

$-->$ Elektrondynamik $arrow(E)(t, arrow(x)), arrow(B)(t, arrow(x))$

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

== PLan der Vorlesung

1. Wiederholung
  - Euklidische Geometrie im $RR^3$, Vektoranalysis (Differentialformen)
2. Spezielle Relativitätstheorie
  - (Psuedo-) Euklidische Geometrie des Minkowski-Raum $RR^(3, 1)$
3. Maxwell's Theorie
4. Anwendungen
  1. Elektrostatik
  2. Magnetostatik
  3. Elektro- und Magnetostatik in Materie
