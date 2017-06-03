:- module(prologmud_schema, [
              prologmud_init/1,
              default_prologmudenance_graph/1,
              log_start_activity/3,
              log_end_activity/3,
              log_entity_use/2,
              log_entity_create/2,
              prologmud_uri/3,
              xsd_now/1
          ]).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_persistency)).

:- use_module(library(git)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(uri)).
:- use_module(library(version)).

/** <module> Prologmudide PROLOGMUD schema, namespace and visualization hooks.

This module prologmudides the PROLOGMUD schema and the prefix =prologmud= for use in
Prolog.
*/

:- rdf_register_ns(prologmudx,  'http://cliopatria.swi-prolog.org/ns/prologmud#').
:- rdf_register_ns(doap,   'http://usefulinc.com/ns/doap#').

:- rdf_attach_library(prologmud(rdf)).
:- rdf_load_library(prologmud).

:- dynamic
    current_prologmud_uri/2.

:- rdf_meta
    default_prologmudenance_graph(r),
    current_prologmud_uri(r,r).

default_prologmudenance_graph(prologmudbundle).

%!  prologmud_init(+Options) is det.
%
%   Clear current_prologmud_uri cached assertions
%   and initialize prologmud bundle
prologmud_init(Options) :-
    retractall(current_prologmud_uri(_,_)),
    default_prologmudenance_graph(DefaultBundle),
    option(prologmud(PrologmudBundle), Options, DefaultBundle),
    option(persistency(Persistency), Options, false),
    rdf_unload_graph(PrologmudBundle),
    rdf_persistency(PrologmudBundle, Persistency),
    rdf_assert('', rdf:type, prologmud:'Bundle', PrologmudBundle),
    prologmud_uri(PrologmudBundle, program(Program), Options),
    prologmud_uri(PrologmudBundle, person(Person), Options),
    rdf_assert('', prologmud:wasAttributedTo, Person, PrologmudBundle),
    rdf_assert('', prologmud:wasAttributedTo, Program, PrologmudBundle).


%!  prologmud_uri(+Graph, -URI, +Options) is det.
%
%   URI is unified with the URI associated with the current
%   SoftwareAgent (program) or Person in the prologmud bundle Graph.
prologmud_uri(Graph, UriTerm, _Options) :-
    current_prologmud_uri(Graph, UriTerm),!.

prologmud_uri(Graph, program(Program), Options) :-
    prologmud_program(Graph, Program, Options), !.

prologmud_uri(Graph, person(Person), Options) :-
    prologmud_person(Graph, Person, Options), !.


prologmud_program(Graph, Program, Options)  :-
    (   current_prolog_flag(version_git, PL_version)
    ->  true
    ;   current_prolog_flag(version, PL_version)
    ),
    current_prolog_flag(executable, PrologExec),
    uri_file_name(PrologExecURL, PrologExec),
    working_directory(CWD,CWD),
    uri_file_name(CWDF,CWD),
    gethostname(LocalHost),
    variant_sha1(CWDF-LocalHost, LocHash),
    rdf_global_id(prologmudx:LocHash, Location),
    format(atom(LocationLabel), '~w:~w', [LocalHost, CWD]),
    findall(M-U-V-F,
            (   git_module_property(M, home_url(U)),
                git_module_property(M, version(V)),
                git_module_property(M, directory(D)),
                uri_file_name(F, D)
            ),
            MUVs
           ),
    Prolog = 'swi-prolog'-'http://www.swi-prolog.org'-PL_version-PrologExecURL,
    AllModules = [Prolog|MUVs],
    sort(AllModules, SortedModules),
    variant_sha1(SortedModules, Hash),
    rdf_global_id(prologmudx:Hash, Program),
    assert(current_prologmud_uri(Graph, program(Program))),
    option(program_label(Label:Lang), Options, 'Local ClioPatria instance':en),
    rdf_assert(Program, rdfs:label, Label@Lang, Graph),
    rdf_assert(Program, rdf:type,   prologmud:'SoftwareAgent', Graph),
    rdf_assert(Program, prologmud:atLocation, Location, Graph),
    rdf_assert(Location, rdf:type, prologmud:'Location', Graph),
    rdf_assert(Location, prologmudx:host, LocalHost^^xsd:string, Graph),
    rdf_assert(Location, prologmudx:cwd, CWDF, Graph),
    rdf_assert(Location, rdfs:label, LocationLabel^^xsd:string, Graph),
    forall(member(M-U-V-D, SortedModules),
           (   variant_sha1(M-U-V-D-comp, CompHash),
               variant_sha1(M-U-V-D-vers, VersHash),
               rdf_global_id(prologmudx:CompHash, Comp),
               rdf_global_id(prologmudx:VersHash, Version),
               format(atom(VLabel), '~w (~w)', [M, V]),
               rdf_assert(Comp,    rdf:type, doap:'Project', Graph),
               rdf_assert(Version, rdf:type, doap:'Version', Graph),
               rdf_assert(D,       rdf:type, doap:'GitBranch', Graph),
               rdf_assert(Program, prologmudx:component, Comp, Graph),
               rdf_assert(Version, doap:revision, V^^xsd:string, Graph),
               rdf_assert(Version, rdfs:label, VLabel^^xsd:string, Graph),
               rdf_assert(Comp, doap:name, M@en, Graph),
               rdf_assert(Comp, doap:repository, D, Graph),
               rdf_assert(Comp, doap:homepage, U, Graph),
               rdf_assert(Comp, doap:release, Version, Graph),
               prologmud_module_settings(Comp, M, Options)
           )
          ),
    !.

prologmud_person(Graph, Person, Options) :-
    default_user_name(DefaultUserName),
    option(user(UserName), Options, DefaultUserName),
    variant_sha1(UserName, Hash),
    rdf_global_id(prologmudx:Hash, DefaultPerson),
    option(person(Person), Options, DefaultPerson),
    rdf_assert(Person, foaf:name, UserName^^xsd:string, Graph),
    rdf_assert(Person, rdf:type, prologmud:'Person', Graph),
    assert(current_prologmud_uri(Graph, person(Person))).

default_user_name(UserName) :-
    git(['config','--get','user.name'], [output(Codes)]),
    atom_codes(Atom, Codes),
    normalize_space(atom(UserName), Atom),
    !.


xsd_now(TimeStamp) :-
    get_time(Time),
    xsd_timestamp(Time, TimeStamp).

xsd_timestamp(Time, TimeStamp) :-
    stamp_date_time(Time, Date, 'UTC'),
    format_time(atom(TimeStamp), '%FT%T%:z', Date, posix).

log_start_activity(Activity, PrologmudBundle, Options0) :-
    option(label(Label), Options0),
    (   ground(PrologmudBundle)
    ->  true
    ;   default_prologmudenance_graph(DefaultBundle),
        option(prologmud(PrologmudBundle), Options0, DefaultBundle)
    ),
    Options = [prologmud(PrologmudBundle) | Options0],
    prologmud_uri(PrologmudBundle, program(Program), Options),
    prologmud_uri(PrologmudBundle, person(Person), Options),
    xsd_now(TimeStamp),
    variant_sha1(TimeStamp:Program:Person:Label, Hash),
    rdf_global_id(prologmudx:Hash, Activity),
    rdf_assert(Activity, rdf:type, prologmud:'Activity', PrologmudBundle),
    rdf_assert(Activity, rdfs:label, Label@en, PrologmudBundle),
    rdf_assert(Activity, prologmud:startedAtTime, TimeStamp^^xsd:dateTime, PrologmudBundle),
    rdf_assert(Activity, prologmud:wasAssociatedWith, Person, PrologmudBundle),
    rdf_assert(Activity, prologmud:wasAssociatedWith, Program, PrologmudBundle).

log_end_activity(Activity, PrologmudBundle, _Options) :-
    xsd_now(TimeStamp),
    rdf_assert(Activity, prologmud:endedAtTime, TimeStamp^^xsd:dateTime, PrologmudBundle).

log_entity_use(Spec, Options) :-
    default_prologmudenance_graph(DefaultBundle),
    option(prologmud(PrologmudBundle), Options, DefaultBundle),
    option(activity(Activity), Options),
    spec_entity_file(Spec, Entity, File),
    rdf_assert(Entity, rdf:type, prologmud:'Entity', PrologmudBundle),
    rdf_assert(Activity, prologmud:used, Entity, PrologmudBundle),
    (   access_file(File, read)
    ->  size_file(File, Size),
        time_file(File, Time),
        xsd_timestamp(Time, Stamp),
        rdf_assert(Entity, prologmudx:file_size, Size^^xsd:integer, PrologmudBundle),
        rdf_assert(Entity, prologmud:generatedAtTime, Stamp^^xsd:dateTime, PrologmudBundle)
    ;   true
    ).

spec_entity_file(Spec, Entity, File) :-
    uri_is_global(Spec),
    Spec = Entity,
    uri_file_name(Entity, File),
    !.

spec_entity_file(Spec, Entity, false) :-
    uri_is_global(Spec),
    Spec = Entity,
    !.

spec_entity_file(Spec, Entity, File) :-
    uri_file_name(Entity, Spec),
    Spec = File,
    !.

log_entity_create(File, Options) :-
    option(activity(Activity), Options),
    option(prologmud(PrologmudBundle), Options),
    option(graph(Graph), Options, none),
    (   uri_is_global(File)
    ->  Entity = File
    ;   uri_file_name(Entity, File)
    ),
    xsd_now(TimeStamp),
    rdf_assert(Entity, rdf:type, prologmud:'Entity', PrologmudBundle),
    rdf_assert(Entity, prologmud:generatedAtTime, TimeStamp^^xsd:dateTime,  PrologmudBundle),
    rdf_assert(Entity, prologmud:wasGeneratedBy, Activity, PrologmudBundle),
    log_derivation(Entity, Options),
    log_entity_graph_properties(Entity, Graph, PrologmudBundle).

log_derivation(Entity, Options) :-
    option(was_derived_from(Sources), Options),!,
    option(prologmud(PrologmudBundle), Options),
    forall(member(Source, Sources),
           (   uri_file_name(SourceUri, Source),
               rdf_assert(Entity, prologmud:wasDerivedFrom, SourceUri, PrologmudBundle)
           )
          ).

log_derivation(_,_). % skip

prologmud_module_settings(Comp, Module, Options) :-
    option(prologmud(PrologmudBundle), Options),
    forall(setting(Module:Key, Value),
           assert_key_value_pair(Comp, Key, Value, PrologmudBundle)
          ).
log_entity_graph_properties(Entity, Graph, PrologmudBundle) :-
    forall(rdf_graph_property(Graph, Property),
           (   Property =.. [ Local, LValue ],
               assert_key_value_pair(Entity, Local, LValue, PrologmudBundle)
           )).

assert_key_value_pair(Entity, Key0, Value0, Graph) :-
    rdf_equal(xsd:string, XsdString),
    (   Key0 = triples
    ->  rdf_global_id(void:triples, Pred)
    ;   Key0 = source_last_modified
    ->  rdf_global_id(dcterms:modified, Pred)
    ;   Key0 = source
    ->  rdf_global_id(prologmud:wasDerivedFrom, Pred)
    ;   rdf_global_id(prologmudx:Key0, Pred)
    ),
    (   Key0 = source_last_modified
    ->  xsd_timestamp(Value0, Value)
    ;   Value0 == []
    ->  rdf_equal(rdf:nil, Value)
    ;   compound(Value0)
    ->  format(atom(Atom), '~p', [Value0]),
        Value = Atom^^XsdString
    ;   ( number(Value0) ; uri_is_global(Value0) )
    ->  Value = Value0
    ;   Value = Value0^^XsdString
    ),
    rdf_assert(Entity, Pred, Value, Graph).
