% David Antunes n107061
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

% eventosSemSalas(EventosSemSala) eh verdade se EventosSemSala eh uma lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala
eventosSemSalas(EventosSemSala) :-  
	findall(ID, evento(ID, _, _, _, semSala), EventosSemSala).

% eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) eh verdade se EventosSemSala eh uma lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala que decorrem em DiaDaSemana
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-  
	findall(ID, (evento(ID, _, _, _, semSala), horario(ID, DiaDaSemana, _, _, _, _)), EventosSemSala).

% acrescenta_semestre/2 eh um predicado auxiliar, e eh verdade se L1 eh uma lista de periodos e L2 eh uma lista com os mesmos periodos e com os semestres que correspondem a esses periodos (p1_2 e/ou p3_4)
acrescenta_semestre(L1, L2) :- (member(p1, L1); member(p2, L1)), (member(p3, L1); member(p4, L1)), append([p1_2, p3_4], L1, L2).
acrescenta_semestre(L1, L2) :- (member(p1, L1); member(p2, L1)), append([p1_2], L1, L2).
acrescenta_semestre(L1, L2) :- (member(p3, L1); member(p4, L1)), append([p3_4], L1, L2).

% eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) eh verdade se ListaPeriodos eh uma lista de periodos e EventosSemSala eh uma lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala nos periodos de ListaPeriodos
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :-  
	acrescenta_semestre(ListaPeriodos, ListaPeriodos2),
	findall(ID, (evento(ID, _, _, _, semSala), member(Periodo, ListaPeriodos2), horario(ID, _, _, _, _, Periodo)) , EventosSemSala).
eventosSemSalasPeriodo([], []).

% organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) eh verdade se EventosNoPeriodo eh a lista, ordenada e sem elementos repetidos, de IDs dos eventos de ListaEventos que ocorrem no periodo Periodo
organizaEventos([], _, []) :- !.  % base de recurcao 
organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :-  % regra que garante que a lista final fica ordenada
	sort(ListaEventos, ListaEventos2), 
	ListaEventos \= ListaEventos2, !,
	organizaEventos(ListaEventos2, Periodo, EventosNoPeriodo).
organizaEventos([H|T1], P, [H|T2]) :-  % se o evento ocorrer no periodo P
	acrescenta_semestre([P], ListaPeriodos),
	member(P2, ListaPeriodos),
	horario(H, _, _, _, _, P2), !, 
	organizaEventos(T1, P, T2).
organizaEventos([_|T1], Periodo, EventosNoPeriodo) :-  % se o evento nao ocorrer no periodo P
	organizaEventos(T1, Periodo, EventosNoPeriodo), !.

% eventosMenoresQue(Duracao, ListaEventosMenoresQue) eh verdade se ListaEventosMenoresQue eh a lista ordenada e sem elementos repetidos dos identificadores dos eventos que tem duracao menor ou igual a Duracao
eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-  
	findall(ID, (horario(ID, _, _, _, Duracao2, _), not(Duracao2 > Duracao)), ListaEventosMenoresQue).

% eventosMenoresQueBool(ID, Duracao) eh verdade se o evento identificado por ID tiver duracao igual ou menor a Duracao.
eventosMenoresQueBool(ID, Duracao) :-  % eh verdade se o evento identificado por ID tiver duracao igual ou menor a Duracao.
	eventosMenoresQue(Duracao, ListaEventosMenoresQue), member(ID, ListaEventosMenoresQue).

% procuraDisciplinas(Curso, ListaDisciplinas) eh verdade se ListaDisciplinas eh a lista ordenada alfabeticamente do nome das disciplinas do curso Curso.
procuraDisciplinas(Curso, ListaDisciplinas) :-  
	findall(Disciplina, (turno(ID, Curso, _, _), evento(ID, Disciplina, _, _, _)), ListaDisciplinas1), sort(ListaDisciplinas1, ListaDisciplinas).

% organizaDisciplinas(ListaDisciplinas, Curso, Semestres) eh verdade se Semestres eh uma lista com duas listas. A lista na primeira posicao contem as disciplinas de ListaDisciplinas do curso Curso que ocorrem no primeiro semestre e a lista na segunda posicao contem as que ocorrem no segundo semestre.
organizaDisciplinas([], _, [[], []]).  % base de recurcao
organizaDisciplinas([H|T1], Curso, [S1, S2]) :-  % a disciplina pertence ao primeiro semestre
	evento(ID, H, _, _, _),
	turno(ID, Curso, _, _),
	horario(ID, _, _, _, _, Periodo),
	member(Periodo, [p1, p2, p1_2]), !,
	append([H], Novo_s1, S1),
	organizaDisciplinas(T1, Curso, [Novo_s1, S2]).
organizaDisciplinas([H|T1], Curso, [S1, S2]) :-  % a disciplina pertence ao segundo semestre
	evento(ID, H, _, _, _), 
	turno(ID, Curso, _, _),
	horario(ID, _, _, _, _, Periodo), 
	member(Periodo, [p3, p4, p3_4]), !, 
	append([H], Novo_s2, S2), 
	organizaDisciplinas(T1, Curso, [S1, Novo_s2]).

% horasCurso(Periodo, Curso, Ano, TotalHoras) eh verdade se TotalHoras for o numero de horas total dos eventos associadas ao curso Curso, no ano Ano e periodo Periodo
horasCurso(Periodo, Curso, Ano, TotalHoras) :-  
	acrescenta_semestre([Periodo], ListaPeriodos),
	findall(ID, turno(ID, Curso, Ano, _), ListaIDs),
	sort(ListaIDs, ListaIDs2),
	findall(Duracao, (member(ID, ListaIDs2), horario(ID, _, _, _, Duracao, Periodo2), member(Periodo2, ListaPeriodos)), ListaHoras), 
	sum_list(ListaHoras, TotalHoras). 

% evolucaoHorasCurso(Curso, Evolucao) eh verdade se Evolucao for uma lista de tuplos na forma (Ano, Periodo, NumHoras), em que NumHoras eh o total de horas associadas ao curso Curso, no ano Ano e periodo Periodo
evolucaoHorasCurso(Curso, Evolucao) :-  
	findall((Ano, Periodo, NumHoras), (member(Ano, [1, 2, 3]), horasCurso(Periodo, Curso, Ano, NumHoras)), Evolucao). 

% ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) eh verdade se Horas for o numero de horas sobrepostas entre o evento que tem inicio em HoraInicioEvento e fim em HoraFimEvento, e o slot que tem inicio em HoraInicioDada e fim em HoraFimDada
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
	HoraInicioDada =< HoraInicioEvento, HoraFimDada >= HoraFimEvento, Horas is HoraFimEvento - HoraInicioEvento.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
	HoraInicioDada >= HoraInicioEvento, HoraFimDada =< HoraFimEvento, Horas is HoraFimDada - HoraInicioDada.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
	HoraInicioDada < HoraInicioEvento, HoraFimDada < HoraFimEvento, HoraFimDada > HoraInicioEvento, Horas is HoraFimDada - HoraInicioEvento.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
	HoraInicioDada > HoraInicioEvento, HoraFimDada > HoraFimEvento, HoraFimEvento > HoraInicioDada, Horas is HoraFimEvento - HoraInicioDada.

% numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) eh verdade se SomaHoras for o numero de horas ocupadas nas salas do tipo TipoSala, no intervalo de tempo definido entre HoraInicio e HoraFim, no dia da semana DiaSemana, e no periodo Periodo
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-  
	salas(TipoSala, ListaSalas),
	acrescenta_semestre([Periodo], ListaPeriodos), 
	findall((ID, Horas), (member(Sala, ListaSalas), evento(ID, _, _, _, Sala), horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo2), member(Periodo2, ListaPeriodos), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaIDHoras),
	sort(ListaIDHoras, ListaIDHoras2),
	findall(Horas, member((_, Horas), ListaIDHoras2), ListaHoras),
	sum_list(ListaHoras , SomaHoras).

% ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) eh verdade se Max for o numero de horas possiveis de ser ocupadas por salas do tipo TipoSala (ver acima), no intervalo de tempo definido entre HoraInicio e HoraFim
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-  
	salas(TipoSala, ListaSalas), length(ListaSalas, NumSalas), Max is (HoraFim - HoraInicio) * NumSalas.

% percentagem(SomaHoras, Max, Percentagem) eh verdade se Percentagem for a divisao de SomaHoras por Max, multiplicada por 100
percentagem(SomaHoras, Max, Percentagem) :-  
	Percentagem is SomaHoras / Max * 100.

% ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) eh verdade se Resultados for uma lista ordenada de tuplos do tipo casosCriticos(DiaSemana, TipoSala, Percentagem) em que DiaSemana, TipoSala e Percentagem sao, respectivamente, um dia da semana, um tipo de sala e a sua percentagem de ocupacao, no intervalo de tempo entre HoraInicio e HoraFim, e supondo que a percentagem de ocupacao relativa a esses elementos esta acima de um dado valor critico (Threshold)
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :- 
	findall(casosCriticos(DiaSemana, TipoSala, Percentagem), 
	(member(DiaSemana, [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira, sabado, domingo]),
	numHorasOcupadas(_, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras), 
	ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), 
	percentagem(SomaHoras, Max, PercentagemExata),
	PercentagemExata > Threshold,
	ceiling(PercentagemExata, Percentagem)),
	ResultadosAux), 
	sort(ResultadosAux, Resultados).

% ocupacaoMesa(ListaNomes, ListaRestricoes, OcupacaoMesa) eh verdade se OcupacaoMesa for a distribuicao dos nomes da ListaNomes que verifica todas as restricoes da ListaRestricoes
ocupacaoMesa(ListaNomes, [], [[A,B,C],[D,E],[F,G,H]]) :-  % se ja nao houver restricoes e os lugares nao estiverem todos ocupados
	member(A, ListaNomes), member(B, ListaNomes), member(C, ListaNomes), member(D, ListaNomes), 
	member(E, ListaNomes), member(F, ListaNomes), member(G, ListaNomes), member(H, ListaNomes), 
	list_to_set([A,B,C,D,E,F,G,H],[A,B,C,D,E,F,G,H]).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [cab1, NomePessoa], member(NomePessoa, ListaPessoas),   
	L = [[_,_,_],[NomePessoa,_],[_,_,_]],
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [cab2, NomePessoa], member(NomePessoa, ListaPessoas), 
	L = [[_,_,_],[_,NomePessoa],[_,_,_]],
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [honra, NomePessoa1, NomePessoa2], member(NomePessoa1, ListaPessoas), member(NomePessoa2, ListaPessoas),
	(L = [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]]; 
	L = [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]]),
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [lado, NomePessoa1, NomePessoa2], member(NomePessoa1, ListaPessoas), member(NomePessoa2, ListaPessoas),
	(L = [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]]; 
	L = [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]];
	L = [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]];
	L = [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]];
	L = [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]];
	L = [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]];
	L = [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]];
	L = [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]),
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [naoLado, NomePessoa1, NomePessoa2], member(NomePessoa1, ListaPessoas), member(NomePessoa2, ListaPessoas),
	(L =[[_,_,_],[NomePessoa1,_],[_,_,_]]; 
	L = [[_,_,_],[NomePessoa2,_],[_,_,_]];
	L = [[_,_,_],[_,NomePessoa1],[_,_,_]];
	L = [[_,_,_],[_,NomePessoa2],[_,_,_]];
	L = [[NomePessoa1,_,NomePessoa2],[_,_],[_,_,_]];
	L = [[NomePessoa2,_,NomePessoa1],[_,_],[_,_,_]];
	L = [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]];
	L = [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]];
	L = [[NomePessoa1,_,_],[_,_],[_,NomePessoa2,_]];
	L = [[NomePessoa2,_,_],[_,_],[_,NomePessoa1,_]];
	L = [[NomePessoa1,_,_],[_,_],[_,_,NomePessoa2]];
	L = [[NomePessoa2,_,_],[_,_],[_,_,NomePessoa1]];
	L = [[_,NomePessoa1,_],[_,_],[NomePessoa2,_,_]];
	L = [[_,NomePessoa2,_],[_,_],[NomePessoa1,_,_]];
	L = [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]];
	L = [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]];
	L = [[_,NomePessoa1,_],[_,_],[_,_,NomePessoa2]];
	L = [[_,NomePessoa2,_],[_,_],[_,_,NomePessoa1]];
	L = [[_,_,NomePessoa1],[_,_],[NomePessoa2,_,_]];
	L = [[_,_,NomePessoa2],[_,_],[NomePessoa1,_,_]];
	L = [[_,_,NomePessoa1],[_,_],[_,NomePessoa2,_]];
	L = [[_,_,NomePessoa2],[_,_],[_,NomePessoa1,_]];
	L = [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]];
	L = [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]];
	L = [[_,_,_],[_,_],[NomePessoa1,_,NomePessoa2]];
	L = [[_,_,_],[_,_],[NomePessoa2,_,NomePessoa1]]),
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [frente, NomePessoa1, NomePessoa2], member(NomePessoa1, ListaPessoas), member(NomePessoa2, ListaPessoas),
	(L = [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]];
	L = [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]];
	L = [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]];
	L = [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]];
	L = [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]];
	L = [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]]),
	ocupacaoMesa(ListaPessoas, T, L).
ocupacaoMesa(ListaPessoas, [H|T], L) :- H =.. [naoFrente, NomePessoa1, NomePessoa2], member(NomePessoa1, ListaPessoas), member(NomePessoa2, ListaPessoas),
	(L = [[_,_,_],[NomePessoa1,_],[_,_,_]]; 
	L = [[_,_,_],[NomePessoa2,_],[_,_,_]];
	L = [[_,_,_],[_,NomePessoa1],[_,_,_]];
	L = [[_,_,_],[_,NomePessoa2],[_,_,_]];
	L = [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]];
	L = [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]];
	L = [[NomePessoa1,_,NomePessoa2],[_,_],[_,_,_]];
	L = [[NomePessoa2,_,NomePessoa1],[_,_],[_,_,_]];
	L = [[NomePessoa1,_,_],[_,_],[_,NomePessoa2,_]];
	L = [[NomePessoa2,_,_],[_,_],[_,NomePessoa1,_]];
	L = [[NomePessoa1,_,_],[_,_],[_,_,NomePessoa2]];
	L = [[NomePessoa2,_,_],[_,_],[_,_,NomePessoa1]];
	L = [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]];
	L = [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]];
	L = [[_,NomePessoa1,_],[_,_],[NomePessoa2,_,_]];
	L = [[_,NomePessoa2,_],[_,_],[NomePessoa1,_,_]];
	L = [[_,NomePessoa1,_],[_,_],[_,_,NomePessoa2]];
	L = [[_,NomePessoa2,_],[_,_],[_,_,NomePessoa1]];
	L = [[_,_,NomePessoa1],[_,_],[NomePessoa2,_,_]];
	L = [[_,_,NomePessoa2],[_,_],[NomePessoa1,_,_]];
	L = [[_,_,NomePessoa1],[_,_],[_,NomePessoa1,_]];
	L = [[_,_,NomePessoa2],[_,_],[_,NomePessoa1,_]];
	L = [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]];
	L = [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]];
	L = [[_,_,_],[_,_],[NomePessoa1,_,NomePessoa2]];
	L = [[_,_,_],[_,_],[NomePessoa2,_,NomePessoa1]];
	L = [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]];
	L = [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]),
	ocupacaoMesa(ListaPessoas, T, L).


	
