# Cosa c'e' in questa repository

Nella cartella `src` sono presenti i file `Analysys.hs`, che contiene le funzioni usate per processare i file di dati grezz,
e `LabParameters.hs` in cui sono preseti eventuali parametri (sperimentali o teorici) che avevamo assunto in laboratorio.

Nella cartella `app` e' presente solo il file `Main.hs` che contiene il procedimento di analisi.

## Main

Dalla funzione `processFile` vengono chiamate le funzioni presenti in `Analysys.hs` e mostra in output i risultati di ogni signola misura,
la migliore stima della quantita' cercata con relativa incertezza, e compie un test della t di Student tra due possibilita': `ConfidenceInterval` che 
restituisce l'intervallo di fiducia definito dal livello di confidenza passato come parametro, oppure `SignificanceTest` che restituisce un livello di compatibilita'
con un valore atteso tramite un test a due code.

I dati processati poi vengono utilizzati eventualmente per altri scopi nel main.

## Plotting

Nella cartella `plotting` e' presente il codice in C++ usato per plottare i dati delle regressioni lineari. I grafici sono salvati nella medesima cartella
