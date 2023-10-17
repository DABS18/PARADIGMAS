| package |
package := Package name: 'Trabajo Práctico G9'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AltaComplejidad;
	add: #Intervencion;
	add: #IntervencionRegistrada;
	add: #Medico;
	add: #Paciente;
	add: #Sanatorio;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Intervencion
	instanceVariableNames: 'codigo descripcion especialidad arancel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #IntervencionRegistrada
	instanceVariableNames: 'fecha medico paciente intervencion condicionPago'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Medico
	instanceVariableNames: 'nombre apellido matricula especialidad condicion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Paciente
	instanceVariableNames: 'dni nombre apellido obraSocial porcCobertura'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sanatorio
	instanceVariableNames: 'medico paciente intervencion intervencionPaciente'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Intervencion subclass: #AltaComplejidad
	instanceVariableNames: ''
	classVariableNames: 'Adicional'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Intervencion guid: (GUID fromString: '{78ebfb89-807a-448c-bff1-d14495fd402d}')!
Intervencion comment: ''!
!Intervencion categoriesForClass!Kernel-Objects! !
!Intervencion methodsFor!

arancel
^arancel!

cargaDatos: unCod
|temp|
codigo:=unCod.
descripcion:=(Prompter prompt: 'Ingrese la descripcion').
especialidad:=(Prompter prompt: 'Ingrese la especialidad').
(temp:=(Prompter prompt: 'Ingrese el arancel')).
[((self esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número'.
	temp:=(Prompter prompt: 'Ingrese el arancel').
	((self esFlotante: temp))
].
arancel:=temp asNumber asFloat.

!

codigo
^codigo!

descripcion
^descripcion!

esFlotante: unNumero
    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp!

especialidad
^especialidad!

muestra
Transcript cr; show: codigo ; tab; tab; show:descripcion ; tab;tab;show:especialidad;tab;tab;show: arancel printString.
(MessageBox notify: 'DESCRIPCIÓN		', descripcion , '
',
'ESPECIALIDAD		', especialidad ,'
','
','ARANCEL			', arancel printString caption: 'Búsqueda de intervenciones > Intervención ',codigo).!

precargaDatos: unCod y: unaDesc y: unaEsp y: unArancel
	codigo := unCod.
	descripcion := unaDesc.
	especialidad := unaEsp.
	arancel := unArancel.! !
!Intervencion categoriesForMethods!
arancel!public! !
cargaDatos:!public! !
codigo!public! !
descripcion!public! !
esFlotante:!public! !
especialidad!public! !
muestra!public! !
precargaDatos:y:y:y:!public! !
!

!Intervencion class methodsFor!

esFlotante: unNumero
    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp! !
!Intervencion class categoriesForMethods!
esFlotante:!public! !
!

IntervencionRegistrada guid: (GUID fromString: '{e572bfc1-8db5-432d-a93c-cb49ed4d6a0b}')!
IntervencionRegistrada comment: ''!
!IntervencionRegistrada categoriesForClass!Kernel-Objects! !
!IntervencionRegistrada methodsFor!

cargaDatos: unaFecha y: unPaciente y: unMedico y:unaIntervencion y:unaCondicion

fecha:=unaFecha.
paciente:=unPaciente.
medico:=unMedico.
intervencion:=unaIntervencion.
condicionPago:= unaCondicion.!

condicionPago
^condicionPago!

fecha
^fecha!

intervencion
^intervencion!

medico
^medico!

paciente
^paciente
!

precargaDatos: unaFecha y: unPaciente y: unMedico y: unaIntervencion y: unaCondicion

fecha:=unaFecha.
paciente:=unPaciente.
medico:=unMedico.
intervencion:=unaIntervencion.
condicionPago:= unaCondicion.! !
!IntervencionRegistrada categoriesForMethods!
cargaDatos:y:y:y:y:!public! !
condicionPago!public! !
fecha!public! !
intervencion!public! !
medico!public! !
paciente!public! !
precargaDatos:y:y:y:y:!public! !
!

Medico guid: (GUID fromString: '{2589d0d7-de77-4739-ac65-7d764c177c02}')!
Medico comment: ''!
!Medico categoriesForClass!Kernel-Objects! !
!Medico methodsFor!

apellido
^apellido!

cargaDatos: unaMatricula

matricula:=unaMatricula.
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
especialidad:=(Prompter prompt: 'Ingrese la especialidad').
condicion:=(MessageBox confirm:'¿Está disponible?').!

cargaDatos: unaMatricula y: unaEspecialidad y: unaDisponibilidad

matricula:=unaMatricula.
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
especialidad:=unaEspecialidad.
condicion:=unaDisponibilidad.!

condicion

^condicion!

especialidad

^ especialidad!

matricula
^matricula!

muestra
Transcript cr; show: nombre; tab; tab; show:apellido; tab;tab;show:matricula printString.

(MessageBox notify: 'PROFESIONAL		', nombre, ' ', apellido, '
',
'MATRÍCULA		', matricula, '
',
'ESPECIALIDAD		', especialidad ,'
','
', 
(condicion ifTrue: ['	  ✔️ Disponible '] ifFalse: ['	  ❌No disponible ']) caption:'Resultado encontrado'
)!

nombre
^nombre!

precargaDatos: unaMatricula y: unNombre y: unApellido y: unaEspecialidad y: unaCondicion
matricula:= unaMatricula.
nombre:=unNombre.
apellido:=unApellido.
especialidad:=unaEspecialidad.
condicion:=unaCondicion.! !
!Medico categoriesForMethods!
apellido!public! !
cargaDatos:!public! !
cargaDatos:y:y:!public! !
condicion!public! !
especialidad!public! !
matricula!public! !
muestra!public! !
nombre!public! !
precargaDatos:y:y:y:y:!public! !
!

Paciente guid: (GUID fromString: '{6d238f25-d0b2-42a7-9548-b835c9b80ed3}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

apellido
^apellido!

cargaDatos: unDni

|ob temp|
dni:=unDni.
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
ob:=(MessageBox confirm:'¿Usted posee obra social?').
ob ifTrue: [obraSocial:=(Prompter prompt: 'Ingrese el nombre de su obra social'). 
(temp:=(Prompter prompt: 'Ingrese el porcentaje de cobertura')).
[((self esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número'.
	temp:=(Prompter prompt: 'Ingrese el porcentaje de cobertura').
	((self esFlotante: temp))
].
porcCobertura:=temp asNumber asFloat.
].
ob ifFalse: [obraSocial:='No posee Obra Social'. porcCobertura:=0].
!

dni
^dni!

esFlotante: unNumero
    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp!

muestra

Transcript cr; show: nombre; tab; tab; show:apellido; tab;tab;show:dni printString.
(MessageBox notify: 'PACIENTE		', nombre, ' ', apellido, '
',
'DNI		', dni, '
',
'COBERTURA	', porcCobertura printString , '%' ,'
','
', 
((obraSocial='No posee Obra Social') ifFalse: ['        ✔️ ', obraSocial, '' ] ifTrue: ['    ❌ No tiene Obra Social ']) caption:'Resultado encontrado'
) !

nombre
^nombre!

obraSocial

^obraSocial!

porcCobertura

^porcCobertura!

precargaDatos: unDni y: unNombre y: unApellido y: unaOb y: unPorc
dni:= unDni.
nombre:=unNombre.
apellido:=unApellido.
obraSocial:=unaOb.
porcCobertura:= unPorc.! !
!Paciente categoriesForMethods!
apellido!public! !
cargaDatos:!public! !
dni!public! !
esFlotante:!public! !
muestra!public! !
nombre!public! !
obraSocial!public! !
porcCobertura!public! !
precargaDatos:y:y:y:y:!public! !
!

Sanatorio guid: (GUID fromString: '{bb5d2598-1dbf-4024-8299-8a84559782e8}')!
Sanatorio comment: ''!
!Sanatorio categoriesForClass!Kernel-Objects! !
!Sanatorio methodsFor!

buscarEnColeccion: unValor y: unaColeccion
|temp|

(unaColeccion = intervencion) ifTrue:[
	temp:= unaColeccion detect:[:i | i codigo = unValor]  
].
(unaColeccion = medico) ifTrue: [
	temp:= unaColeccion detect:[:i | i matricula = unValor]  
].
(unaColeccion = paciente) ifTrue: [
	temp:= unaColeccion detect:[:i | i dni = unValor]  
].
^temp!

calcDescuento: unTotal y: unPorcentaje

^(unTotal*unPorcentaje)/100

!

consulta
|op|
op:='5'.
[op='0'] whileFalse: [(MessageBox notify: '1 - Buscar pacientes
2 - Buscar médicos
3. Buscar Intervenciones
0 - Volver al menú' caption: 'Menú > Consultas').
op:=(Prompter prompt: 'Ingrese una opción').
(op='1') ifTrue: [self consultaPaciente].
(op='2') ifTrue: [self consultaMedico].
(op='3') ifTrue: [self consultaIntervencion ].
((((op='1' or: [op='2']) or: [op='0']) or:[op='0']) or: [op='3']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')].
]
!

consultaIntervencion
|cod t|

[t isNil] whileTrue:[
cod:=Prompter prompt: 'Ingrese el código de la intervención' caption:'Consulta > Intervención'.
t:= intervencion detect:[:i | i codigo = cod]
ifNone:[ MessageBox notify: 'Incorrecto. Vuelva a ingresar legajo o escriba SALIR para regresar al menú.'. t:= nil. ((cod='SALIR') ifTrue: [t:='3']) ]].

(t isNil) ifFalse: [t muestra].!

consultaMedico

|mat m|

[m isNil] whileTrue:[
mat:=Prompter prompt: 'Ingrese la matrícula del profesional' caption:'Consulta > Médico'.
m:= medico detect:[:i | i matricula=mat]
ifNone:[ MessageBox notify: 'Incorrecto. Vuelva a ingresar legajo o escriba SALIR para regresar al menú.'. m:= nil. ((mat='SALIR') ifTrue: [m:='3']) ]].

(m isNil) ifFalse: [m muestra].

 !

consultaPaciente

|pac p|

[p isNil] whileTrue:[
pac:=Prompter prompt: 'Ingrese el DNI del paciente' caption:'Consulta > Paciente'.
p:= paciente detect:[:i | i dni=pac ]
ifNone:[ MessageBox notify: 'Incorrecto. Vuelva a ingresar el DNI o escriba SALIR para regresar al menú.'. p:= nil. ((pac='SALIR') ifTrue: [p:='3'])]].

(p isNil) ifFalse: [p muestra].!

esFechaValida: unaFecha
    | fechaHoy fecha temp |
    fecha := [Date fromString: unaFecha format: 'MM/DD/yyyy']  on: Error do: [:each | temp:= false].
    (temp=false) ifTrue: [temp:=false] ifFalse:[temp:=true].
    fechaHoy := Date today.
    ^ (temp=true and: [fecha >= fechaHoy]).
	
  !

estadoliquidacion: unDNI
	|coleccionPaciente coleccion2 tempInt tempMed acumAdic total nodebe|
	total:= 0.
	acumAdic:=0.
	coleccionPaciente:= (self buscarEnColeccion: unDNI y: paciente).	
	coleccion2 := intervencionPaciente select:[:each | each paciente = unDNI and:[each condicionPago = false]].
	Transcript clear.
	Transcript show: 'Liquidación del paciente: ';show: coleccionPaciente nombre;show: ' ';show: coleccionPaciente apellido;show: '  Obra social:  '; show: coleccionPaciente obraSocial; cr.
	Transcript show: 'Fecha';tab; show: 'Descripcion      ';show: 'Medico';tab; show: 'Mat.';tab; show: 'Importe'; cr.
        coleccion2 do: [:i |
	    tempInt:= (self buscarEnColeccion: (i intervencion) y: intervencion).
	    tempMed:= (self buscarEnColeccion: (i medico) y: medico).	
            Transcript show: i fecha;show: '    '; show: tempInt descripcion;show: '   ' ;show: tempMed nombre;show:' ';show: tempMed apellido;show: '   '; show: tempMed matricula;show: '   ';show: '$'; print: tempInt arancel; tab; tab;
                cr.
	(tempInt isKindOf: AltaComplejidad) ifTrue: [total:= total + (tempInt arancel) + ( AltaComplejidad adicional). acumAdic:= AltaComplejidad adicional + acumAdic ] ifFalse: [total:= total + (tempInt arancel)]
        ].
	Transcript cr; show: 'Carga por Adicional';tab;tab;tab;show:'$';print: acumAdic; cr.
	Transcript show: 'Total';tab;tab;tab;tab;show:'$';print: total; cr.
	Transcript show: 'Cobertura Obra social';tab;tab;show: '$';print: (self calcDescuento: total y: coleccionPaciente porcCobertura) ;cr.
        Transcript show: 'Neto a pagar       ';tab;tab;tab;show:'$';print: (self netoaPagar: total y: coleccionPaciente porcCobertura) ;cr.
	
	(total = 0) ifTrue: [^'Este paciente no registra deudas'] ifFalse:[^(Transcript contents) asString].!

existeCOD: unCOD
|i int|
int:= unCOD.
i:= intervencion detect:[:each | each codigo=int] ifNone:[i:= 'no'.].
(i='no') ifTrue: [^false] ifFalse: [^true ]
!

existeDNI: unDNI
| p pac|
pac:= unDNI.
p:= paciente detect:[:i | i dni=pac] ifNone:[p:= 'no'.].
(p='no') ifTrue: [^false] ifFalse: [^true ]
!

existeEspecialidad: unaEspecialidad
|e|
e:= intervencion detect:[:i | i especialidad=unaEspecialidad] ifNone:[e:= 'no'.].
(e='no') ifTrue: [^false] ifFalse: [^true ]!

existeMatricula: unaMatricula
| m med|
med:= unaMatricula.
m:= medico detect:[:i | i matricula=med] ifNone:[m:= 'no'.].
(m='no') ifTrue: [^false] ifFalse: [^true ]!

inicio
	paciente := OrderedCollection new.
	medico := OrderedCollection new.
	intervencion := OrderedCollection new.
	intervencionPaciente:= OrderedCollection new.
	"AltaComplejidad cargaAdicional.

	intervencionPaciente add: (IntervencionRegistrada new
				precargaDatos: '05/23/2026' 
				y: '44765236' 
				y: '50925' 
				y: '21'
				y: false yourself).

	medico add: (Medico new
				precargaDatos: '50925'
				y: 'diego'
				y: 'gonzalez'
				y: 'trauma'
				y: true yourself).

	paciente add: (Paciente new
				precargaDatos: '44765236'
				y: 'Manuel'
				y: 'Variego'
				y: 'OSDE'
				y: 30 yourself).
"
	intervencion add: (Intervencion new
				precargaDatos: '21' 
				y: 'general' 
				y: 'trauma' 
				y: 50 yourself).
"
	intervencion add: (AltaComplejidad new
				precargaDatos: '02' 
				y: 'Trasplante de riñón' 
				y: 'Nefrología'
				y: 25000 	yourself).
				AltaComplejidad precargaAdicional: 15.

	intervencion add: (Intervencion new
				precargaDatos: '03' 
				y: 'Cirugía de cataratas' 
				y: 'Oftalmología' 
				y: 5000 yourself).

	intervencion add: (Intervencion new
				precargaDatos: '04' 
				y: 'Artroscopia de rodilla' 
				y: 'Ortopedia'
				y: 7000 yourself)."
"
	paciente add: (Paciente new
				precargaDatos: '01'
				y: 'Joel'
				y: 'Marchesa'
				y: 'Swiss Medical'
				y: 10 yourself).
	paciente add: (Paciente new
				precargaDatos: '02'
				y: 'Tomas'
				y: 'Messa'
				y: 'No posee Obra Social'
				y: 0 yourself).
	paciente add: (Paciente new
				precargaDatos: '01'
				y: 'Joel'
				y: 'Marchesa'
				y: 'Swiss Medical'
				y: 10 yourself).
	paciente add: (Paciente new
				precargaDatos: '03'
				y: 'Ulises'
				y: 'Gutiérrez'
				y: 'Obra Social: OSDE'
				y: 15 yourself).
	medico add: (Medico new
				precargaDatos: '01'
				y: 'Ramón'
				y: 'Pascual'
				y: 'Nefrología'
				y: true yourself).
	medico add: (Medico new
				precargaDatos: '05'
				y: 'Error'
				y: 'Pascual'
				y: 'Nefrología'
				y: false yourself).
	medico add: (Medico new
				precargaDatos: '02'
				y: 'Valentín'
				y: 'Vidente'
				y: 'Oftalmología'
				y: true yourself).
	medico add: (Medico new
				precargaDatos: '03'
				y: 'Roberto'
				y: 'Neuross'
				y: 'Neurología'
				y: false yourself)
"!

intervencionesDisponibles: unaEspecialidad
|coleccion temp|
coleccion := intervencion select: [:each | each especialidad=unaEspecialidad].
(coleccion isEmpty) 
    ifTrue:[MessageBox notify: 'No hay intervenciones disponibles.' ]
    ifFalse: [
	Transcript clear.
	Transcript show: 'ESPECIALIDAD - '; show: unaEspecialidad asUppercase; cr.
	Transcript show: 'CÓDIGO'; tab;tab; show:'DESCRIPCIÓN';tab;tab;show:'ARANCEL'; cr.
        coleccion do: [:each | 
            Transcript 
                show: each codigo; tab;tab;
		show: each descripcion; tab;tab;
		show: each arancel printString; tab;tab;
                cr.
        ].
	temp:= (Transcript contents) asString.
	^temp
    ]!

liquidacion
|coleccion2 rta dni|

rta:=1.
dni:=(Prompter prompt: 'Ingrese el DNI del paciente con intervenciones registradas').
[rta = 1] whileTrue:[
	coleccion2 := intervencionPaciente select: [:each | each paciente = dni].
	(coleccion2 isEmpty) ifTrue: [
	(MessageBox warning: 'No existe una intervencion pendiente de pago registrado con ese DNI. Intente nuevamente').
] ifFalse: [
	MessageBox notify: (self estadoliquidacion: dni).
].
	dni:=(Prompter prompt: 'Ingrese otro DNI o 0 para salir').
	(dni = '0') ifTrue:[ rta:=0].
].

"messagebox notify: self listado liquidacion"
	"paciente tiene intervencion registrada y condicion pago falso, p es paciente en intervencion registrada"
		"listar paciente y obra social"
		"listar fecha descripcion medico mat e importe"





"
coleccion1 := medico select: [:each | each condicion].
coleccion2:= coleccion1 select: [:each | each especialidad=unaEspecialidad].
(coleccion2 isEmpty) 
    ifTrue:[MessageBox notify: 'No hay médicos disponibles.' ]
    ifFalse: [
	Transcript clear.
	Transcript show: 'ESPECIALIDAD - '; show: unaEspecialidad asUppercase; cr.
	Transcript show: 'MATRÍCULA'; tab; show:'PROFESIONAL'; cr.
        coleccion2 do: [:each | 
            Transcript 
                show: each matricula; tab; tab;
		show: each nombre; show: ' '; show: each apellido; tab; tab;
                cr.
        ].
	temp:= (Transcript contents) asString.
	^temp
    ]"!

medicosDisponibles: unaEspecialidad
|coleccion1 coleccion2 temp|
coleccion1 := medico select: [:each | each condicion].
coleccion2:= coleccion1 select: [:each | each especialidad=unaEspecialidad].
(coleccion2 isEmpty) 
    ifTrue:[MessageBox notify: 'No hay médicos disponibles.' ]
    ifFalse: [
	Transcript clear.
	Transcript show: 'ESPECIALIDAD - '; show: unaEspecialidad asUppercase; cr.
	Transcript show: 'MATRÍCULA'; tab; show:'PROFESIONAL'; cr.
        coleccion2 do: [:each | 
            Transcript 
                show: each matricula; tab; tab;
		show: each nombre; show: ' '; show: each apellido; tab; tab;
                cr.
        ].
	temp:= (Transcript contents) asString.
	^temp
    ]!

medicosDisponibles: unaEspecialidad y: unaOpcion
|coleccion1 coleccion2 temp temp2|
coleccion1 := medico select: [:each | each condicion].
coleccion2:= coleccion1 select: [:each | each especialidad=unaEspecialidad].
(coleccion2 isEmpty) 
    ifTrue:[MessageBox notify: 'No hay médicos disponibles.'. temp2:=false.]
    ifFalse: [
	Transcript clear.
	Transcript show: 'ESPECIALIDAD - '; show: unaEspecialidad asUppercase; cr.
	Transcript show: 'MATRÍCULA'; tab; show:'PROFESIONAL'; cr.
        coleccion2 do: [:each | 
            Transcript 
                show: each matricula; tab; tab;
		show: each nombre; show: ' '; show: each apellido; tab; tab;
                cr.
        ].
	temp2=true.
	temp:= (Transcript contents) asString.
    ].

(unaOpcion=1)ifTrue:[^temp] ifFalse:[^temp2]!

menu
|op|

op:='5'.
[op='0'] whileFalse: [MessageBox notify: '1 - Registrar Intervencion 
2 - Mostrar liquidacion
3. Consultas
0 - Salir' caption: 'MENU DE OPCIONES'.
op:=(Prompter prompt: 'Ingrese una opcion').
(op='1') ifTrue: [self registrarIntervencionPaciente].
(op='2') ifTrue: [self liquidacion].
(op='3') ifTrue: [self consulta].
(op= '/admin') ifTrue: [self menuAdmin].
((((op='1' or: [op='2']) or: [op='/admin']) or:[op='3']) or: [op='0']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')]
]

!

menuAdmin
|op|

op:='4'.
[op='0'] whileFalse: [MessageBox notify: '1 - Registrar paciente
2 - Registrar médico
3 - Registrar intervención
0 - Volver al menú' caption: 'PANEL DE ADMINISTRADOR'.
op:=(Prompter prompt: 'Ingrese una opción:').
(op='1') ifTrue: [self registrarPaciente].
(op= '2') ifTrue: [self registrarMedico].
(op='3') ifTrue: [self registrarIntervencion].
(((op='1' or: [op='2']) or: [op='0']) or: [op='3']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')]
]

!

netoaPagar: unTotal y: unPorcentaje

^(unTotal - (self calcDescuento: unTotal y: unPorcentaje)) !

registrarIntervencion
|rta rta2 t cod|

rta:= true.

(intervencion isEmpty) ifTrue: [AltaComplejidad cargaAdicional].
[rta] whileTrue: [
    cod := (Prompter prompt: 'Ingrese el codigo' caption:'Menú administrador > Registro > Intervención').
    (self existeCOD: cod) ifTrue: [
        MessageBox warning: 'El codigo ya existe. Por favor, ingrese otro.' caption:'Menú administrador > Registro > Intervención'.
    ] ifFalse: [
        rta2 := MessageBox confirm: '¿Es una intervencion de alta complejidad?' caption:'Menú administrador > Registro > Intervención'.
        t := rta2
            ifTrue: [AltaComplejidad new]
            ifFalse: [Intervencion new].
        t cargaDatos: cod .
        intervencion add: t.
        rta:= MessageBox confirm: 'Desea ingresar otra intervencion?' caption:'Menú administrador > Registro > Intervención'
    ]].!

registrarIntervencionPaciente

|rta p fecha inter matricula pac espe|

rta:= true.
(paciente isEmpty or: [intervencion isEmpty or: [medico isEmpty]]) ifTrue: [MessageBox errorMsg: 'BASE DE DATOS VACIA.' caption: 'Error del sistema'] ifFalse: [
[rta] whileTrue: [
    fecha := (Prompter prompt: 'Ingrese una fecha. (MM/DD/YYYY)' caption:'Menú administrador > Registro > Intervención de paciente').
    (self esFechaValida: fecha) ifFalse: [
        MessageBox warning: 'Fecha inválida. Vuelva a intentarlo.' caption:'Menú administrador > Registro > Intervención de paciente'.
    ] ifTrue: [
	pac := (Prompter prompt: 'Ingrese el DNI del paciente' caption:'Menú administrador > Registro > Intervención de paciente').
	[self existeDNI: pac] whileFalse: [
	      pac:= Prompter prompt: 'El documento ingresado no coincide con nuestros registros. Vuelva a intentarlo.' caption:'Menú administrador > Registro > Intervención de paciente'.
	].
	espe:= Prompter prompt: 'Ingrese la especialidad.' caption:'Menú administrador > Registro > Intervención de paciente'.
	[self existeEspecialidad: espe ] whileFalse: [
		espe:= Prompter prompt: 'Los datos ingresados no coinciden con nuestros registros. Vuelva a intentarlo.' caption:'Menú administrador > Registro > Intervención de paciente'.
	].
	MessageBox notify:(self medicosDisponibles: espe).
	matricula:= Prompter prompt: 'Ingrese la matrícula del profesional' caption:'Menú administrador > Registro > Intervención de paciente'.
	[self validarMedico: matricula y: espe] whileFalse: [
		MessageBox warning:('La matrícula ingresada no coincide con nuestros registros. Vuelva a intentarlo.') caption:'Menú administrador > Registro > Intervención de paciente'.
		MessageBox notify:(self medicosDisponibles: espe).
		matricula:= Prompter prompt: 'Ingrese la matrícula del profesional.' caption:'Menú administrador > Registro > Intervención de paciente'.
	].
	MessageBox notify:(self intervencionesDisponibles: espe).
	inter:= Prompter prompt: 'Ingrese el código de intervención' caption:'Menú administrador > Registro > Intervención de paciente'.
	[self validarIntervencion: inter y: espe] whileFalse: [
		MessageBox warning:('El código de intervención ingresado no coincide con nuestros registros. Vuelva a intentarlo.') caption:'Menú administrador > Registro > Intervención de paciente'.
		MessageBox notify:(self intervencionesDisponibles: espe).
		espe:= Prompter prompt: 'Ingrese el código de intervención' caption:'Menú administrador > Registro > Intervención de paciente'.
	].
	
        p:= IntervencionRegistrada new.
        p cargaDatos: fecha y: pac y:matricula y: inter y: (MessageBox confirm: '¿Está pagada?' ).
        intervencionPaciente add: p.
        rta:= MessageBox confirm: '¿Desea registrar otra intervención?' caption:'Menú administrador > Registro > Intervención de paciente'
    ]]].
!

registrarMedico

|rta m matricula especialidad disponibilidad|

rta:= true.

[rta] whileTrue: [
    matricula := (Prompter prompt: 'Ingrese la matrícula' caption:'Menú administrador > Registro > Médico').
    (self existeMatricula: matricula) ifTrue: [
        MessageBox warning: 'La matrícula ya existe. Por favor, ingrese otra.' caption:'Menú administrador > Registro > Médico'.
    ] ifFalse: [
	especialidad := (Prompter prompt: 'Ingrese la especialidad' caption:'Menú administrador > Registro > Médico').
	disponibilidad := (MessageBox confirm: '¿Está disponible?' caption:'Menú administrador > Registro > Médico').
	(((self existeEspecialidad: especialidad)=false and:[disponibilidad=false]) or:[((self medicosDisponibles: especialidad y: 2)=false) and:[disponibilidad=false]]) ifTrue: [
	MessageBox warning: 'Debe haber un médico disponible por cada especialidad' caption:'Menú administrador > Registro > Médico'.
	]
	ifFalse:[
        m:= Medico new.
        m cargaDatos: matricula y: especialidad y: disponibilidad.
        medico add: m.
    ].
	rta:= MessageBox confirm: '¿Desea ingresar otro médico?'  caption:'Menú administrador > Registro > Médico'.
].
].
!

registrarPaciente
|rta p dni|

rta:= true.

[rta] whileTrue: [
    dni := (Prompter prompt: 'Ingrese DNI' caption:'Menú administrador > Registro > Paciente').
    (self existeDNI: dni) ifTrue: [
        MessageBox warning: 'El DNI ya existe. Por favor, ingrese otro.' caption:'Menú administrador > Registro > Paciente'.
    ] ifFalse: [
        p:= Paciente new.
        p cargaDatos: dni .
        paciente add: p.
        rta:= MessageBox confirm: 'Desea ingresar otro paciente?' caption:'Menú administrador > Registro > Paciente'
    ]].!

validarIntervencion: unCodigo y: unaEspec
| m cod inter|
cod:= unCodigo.
m:= intervencion detect:[:i | i codigo=unCodigo and: [i especialidad=unaEspec] ] ifNone:[m:= 'no'.].
(m='no') ifTrue: [^false] ifFalse: [^true ]!

validarMedico: unaMatricula y: unaEspecialidad 
| m med int|
med:= unaMatricula.
int:=unaEspecialidad.
m:= medico detect:[:i | i matricula=med and: [i especialidad=int and: [i condicion=true] ] ] ifNone:[m:= 'no'.].
(m='no') ifTrue: [^false] ifFalse: [^true ]! !
!Sanatorio categoriesForMethods!
buscarEnColeccion:y:!public! !
calcDescuento:y:!public! !
consulta!public! !
consultaIntervencion!public! !
consultaMedico!public! !
consultaPaciente!public! !
esFechaValida:!public! !
estadoliquidacion:!public! !
existeCOD:!public! !
existeDNI:!public! !
existeEspecialidad:!public! !
existeMatricula:!public! !
inicio!public! !
intervencionesDisponibles:!public! !
liquidacion!public! !
medicosDisponibles:!public! !
medicosDisponibles:y:!public! !
menu!public! !
menuAdmin!public! !
netoaPagar:y:!public! !
registrarIntervencion!public! !
registrarIntervencionPaciente!public! !
registrarMedico!public! !
registrarPaciente!public! !
validarIntervencion:y:!public! !
validarMedico:y:!public! !
!

AltaComplejidad guid: (GUID fromString: '{a632a6b3-501a-4a70-abee-034483a530bc}')!
AltaComplejidad comment: ''!
!AltaComplejidad categoriesForClass!Kernel-Objects! !
!AltaComplejidad methodsFor!

muestra
Transcript cr; show: codigo ; tab; tab; show:descripcion ; tab;tab;show:especialidad;tab;tab;show: arancel printString.
(MessageBox notify: 'DESCRIPCIÓN		', descripcion , '
',
'ESPECIALIDAD		', especialidad ,'
','
','ARANCEL			', arancel printString , '
', 'ADICIONAL		', Adicional printString,'%' caption: 'Búsqueda de intervenciones > Intervención ',codigo).! !
!AltaComplejidad categoriesForMethods!
muestra!public! !
!

!AltaComplejidad class methodsFor!

adicional
^Adicional!

cargaAdicional
|temp|
(temp:=(Prompter prompt: 'Ingrese el adicional')).
[((super esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número'.
	temp:=(Prompter prompt: 'Ingrese el adicional').
	((super esFlotante: temp)).
].
Adicional:=temp asNumber asFloat.!

precargaAdicional: unAdic

Adicional:=unAdic.! !
!AltaComplejidad class categoriesForMethods!
adicional!public! !
cargaAdicional!public! !
precargaAdicional:!public! !
!

"Binary Globals"!

