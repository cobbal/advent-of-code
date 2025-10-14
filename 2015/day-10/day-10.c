#include <stdlib.h>
#include <string.h>

#include "common/common.h"

constexpr int nElements = 93;

typedef struct {
    int counts[nElements];
} Molecule;

typedef struct {
    Molecule decays[nElements];
} Decay;

struct {
    char *elementName;
    char *pattern;
    char *decayProducts;
} periodicTable[nElements] = {
    {"Nu", "", ""},
    // From https://en.wikipedia.org/wiki/Look-and-say_sequence#Cosmological_decay
    {"H", "22", "H"},
    {"He", "13112221133211322112211213322112", "Hf.Pa.H.Ca.Li"},
    {"Li", "312211322212221121123222112", "He"},
    {"Be", "111312211312113221133211322112211213322112", "Ge.Ca.Li"},
    {"B", "1321132122211322212221121123222112", "Be"},
    {"C", "3113112211322112211213322112", "B"},
    {"N", "111312212221121123222112", "C"},
    {"O", "132112211213322112", "N"},
    {"F", "31121123222112", "O"},
    {"Ne", "111213322112", "F"},
    {"Na", "123222112", "Ne"},
    {"Mg", "3113322112", "Pm.Na"},
    {"Al", "1113222112", "Mg"},
    {"Si", "1322112", "Al"},
    {"P", "311311222112", "Ho.Si"},
    {"S", "1113122112", "P"},
    {"Cl", "132112", "S"},
    {"Ar", "3112", "Cl"},
    {"K", "1112", "Ar"},
    {"Ca", "12", "K"},
    {"Sc", "3113112221133112", "Ho.Pa.H.Ca.Co"},
    {"Ti", "11131221131112", "Sc"},
    {"V", "13211312", "Ti"},
    {"Cr", "31132", "V"},
    {"Mn", "111311222112", "Cr.Si"},
    {"Fe", "13122112", "Mn"},
    {"Co", "32112", "Fe"},
    {"Ni", "11133112", "Zn.Co"},
    {"Cu", "131112", "Ni"},
    {"Zn", "312", "Cu"},
    {"Ga", "13221133122211332", "Eu.Ca.Ac.H.Ca.Zn"},
    {"Ge", "31131122211311122113222", "Ho.Ga"},
    {"As", "11131221131211322113322112", "Ge.Na"},
    {"Se", "13211321222113222112", "As"},
    {"Br", "3113112211322112", "Se"},
    {"Kr", "11131221222112", "Br"},
    {"Rb", "1321122112", "Kr"},
    {"Sr", "3112112", "Rb"},
    {"Y", "1112133", "Sr.U"},
    {"Zr", "12322211331222113112211", "Y.H.Ca.Tc"},
    {"Nb", "1113122113322113111221131221", "Er.Zr"},
    {"Mo", "13211322211312113211", "Nb"},
    {"Tc", "311322113212221", "Mo"},
    {"Ru", "132211331222113112211", "Eu.Ca.Tc"},
    {"Rh", "311311222113111221131221", "Ho.Ru"},
    {"Pd", "111312211312113211", "Rh"},
    {"Ag", "132113212221", "Pd"},
    {"Cd", "3113112211", "Ag"},
    {"In", "11131221", "Cd"},
    {"Sn", "13211", "In"},
    {"Sb", "3112221", "Pm.Sn"},
    {"Te", "1322113312211", "Eu.Ca.Sb"},
    {"I", "311311222113111221", "Ho.Te"},
    {"Xe", "11131221131211", "I"},
    {"Cs", "13211321", "Xe"},
    {"Ba", "311311", "Cs"},
    {"La", "11131", "Ba"},
    {"Ce", "1321133112", "La.H.Ca.Co"},
    {"Pr", "31131112", "Ce"},
    {"Nd", "111312", "Pr"},
    {"Pm", "132", "Nd"},
    {"Sm", "311332", "Pm.Ca.Zn"},
    {"Eu", "1113222", "Sm"},
    {"Gd", "13221133112", "Eu.Ca.Co"},
    {"Tb", "3113112221131112", "Ho.Gd"},
    {"Dy", "111312211312", "Tb"},
    {"Ho", "1321132", "Dy"},
    {"Er", "311311222", "Ho.Pm"},
    {"Tm", "11131221133112", "Er.Ca.Co"},
    {"Yb", "1321131112", "Tm"},
    {"Lu", "311312", "Yb"},
    {"Hf", "11132", "Lu"},
    {"Ta", "13112221133211322112211213322113", "Hf.Pa.H.Ca.W"},
    {"W", "312211322212221121123222113", "Ta"},
    {"Re", "111312211312113221133211322112211213322113", "Ge.Ca.W"},
    {"Os", "1321132122211322212221121123222113", "Re"},
    {"Ir", "3113112211322112211213322113", "Os"},
    {"Pt", "111312212221121123222113", "Ir"},
    {"Au", "132112211213322113", "Pt"},
    {"Hg", "31121123222113", "Au"},
    {"Tl", "111213322113", "Hg"},
    {"Pb", "123222113", "Tl"},
    {"Bi", "3113322113", "Pm.Pb"},
    {"Po", "1113222113", "Bi"},
    {"At", "1322113", "Po"},
    {"Rn", "311311222113", "Ho.At"},
    {"Fr", "1113122113", "Rn"},
    {"Ra", "132113", "Fr"},
    {"Ac", "3113", "Ra"},
    {"Th", "1113", "Ac"},
    {"Pa", "13", "Th"},
    {"U", "3", "Pa"},
};

static uint8_t atomicNumber(const char *name) {
    for (int i = 1; i < nElements; i++) {
        if (strcmp(name, periodicTable[i].elementName) == 0) {
            return i;
        }
    }
    fprintf(stderr, "Unknown element %s\n", name);
    exit(1);
}

static Decay decayCreate(Arena arena) {
    Decay decay = {};
    for (int i = 0; i < nElements; i++) {
        char *decayStr = arenaStrdup(arena, periodicTable[i].decayProducts);
        char *dot;
        while ((dot = strchr(decayStr, '.'))) {
            *dot = '\0';
            decay.decays[i].counts[atomicNumber(decayStr)]++;
            decayStr = dot + 1;
        }
        if (*decayStr) {
            decay.decays[i].counts[atomicNumber(decayStr)]++;
        }
    }
    return decay;
}

[[maybe_unused]]
static void printMolecule(const Molecule *m, FILE *f) {
    fprintf(f, "(");
    for (int i = 0; i < nElements; i++) {
        if (!m->counts[i]) { continue; }
        fprintf(f, "%s%d ", periodicTable[i].elementName, m->counts[i]);
    }
    fprintf(f, ")\n");
}

static int consumeLongestMatch(char **seq) {
    int best = 0;
    size_t bestLength = SIZE_MAX;
    size_t len = strlen(*seq);
    for (int i = 1; i < nElements; i++) {
        char *pat = periodicTable[i].pattern;
        size_t patLen = strlen(pat);
        if (patLen < len || bestLength <= patLen) { continue; }
        if (strncmp(pat, *seq, patLen) == 0) {
            best = i;
            bestLength = patLen;
        }
    }
    if (best > 0) {
        *seq += bestLength;
    }
    return best;
}

static int64_t solve(Arena arena, FILE *f, int n) {
    Decay decays = decayCreate(arena);
    char *buf = nullptr;
    ssize_t bufLen = 0;
    check(getUntilDelimiter(arena, &buf, &bufLen, '\n', f) > 0);
    Molecule seq = {};
    int match;
    while ((match = consumeLongestMatch(&buf))) {
        seq.counts[match]++;
    }
    for (int iter = 0; iter < n; iter++) {
        Molecule next = {};
        for (int fuel = 1; fuel < nElements; fuel++) {
            if (seq.counts[fuel] == 0) { continue; }
            Molecule decay = decays.decays[fuel];
            for (int daughter = 1; daughter < nElements; daughter++) {
                next.counts[daughter] += seq.counts[fuel] * decay.counts[daughter];
            }
        }
        seq = next;
    }
    int64_t total = 0;
    for (int i = 1; i < nElements; i++) {
        total += seq.counts[i] * strlen(periodicTable[i].pattern);
    }
    return total;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    return solve(arena, f, 40);
}
static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    return solve(arena, f, 50);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-10/input-ex0.txt", solvePart0, 95798, solvePart1, 1355550);
    failed += checkInputInt("day-10/input-real0.txt", solvePart0, 492982, solvePart1, 6989950);
    return failed;
}

daySolver day10 = {10, dayMain, true};
