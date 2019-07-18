# Some example positions and puzzles

This should be a mate in 1 for blue and a mate in 2 for red.
Both of these sequences are "chasing", i.e. the enemy is always put back in ŝako.

Red: `Ndxf6 *Qc6+ Kd8, Qd7#` or some other variant.

Blue: `Qg2#`

This is the first time that I actually had to use the more specific chess notation Ndxf6 :-)

    ╔═══════════════════════════╗
    ║ 8 .R .. .. .. .K .B .. .. ║
    ║ 7 .. .. .. N. .P .P .. .P ║
    ║ 6 .. .P .. .. .. QP PR .. ║
    ║ 5 B. PP .P .B RP .. .. .. ║
    ║ 4 P. BN .. .. N. .. .. .. ║
    ║ 3 .. .. .. P. .. .. .. .Q ║
    ║ 2 .. .. P. PN .. P. .. P. ║
    ║ 1 .. .. .. .. .. R. .. K. ║
    ║ * A  B  C  D  E  F  G  H  ║
    ╚═══════════════════════════╝

This next example is constructed, and I expect to find a mate in 4 for red.
Red jumps `Qxc7 *Bxg3 *Bxc7 *Qxg3 *Bxc7 *Bxg3 *Qh4+` and I expect that
Blue should answer with `Kf8`. Red gives chase `Qh8+ Ke7` and then chains
`Pxg *Bxc7 *Bxd8+`. Here I believe that both `RBg8` and `NBe8` can cut the
chase. Looks like my initial assumption was wrong.

This is still a very interesting position and I would like to explore it
further, once I develop the tools for this.

    ╔═══════════════════════════╗
    ║ 8 .. .. .. .R .. .. .K .. ║
    ║ 7 .. .. BN Q. .. .P .P .. ║
    ║ 6 .. NR .P .. .. NQ PP .. ║
    ║ 5 .. .P .. .. .. .B .. .. ║
    ║ 4 .. PP .. .. PP .. PP .. ║
    ║ 3 .. .. R. .. .. .. BB .. ║
    ║ 2 P. K. P. .. .. P. .. .. ║
    ║ 1 .. .. .. .. R. PN .. .. ║
    ║ * A  B  C  D  E  F  G  H  ║
    ╚═══════════════════════════╝