﻿| package |
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
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Intervencion
	instanceVariableNames: 'codigo descripcion especialidad arancel'
	classVariableNames: 'Contador'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #IntervencionRegistrada
	instanceVariableNames: 'fecha datosMedico datosPaciente datosIntervencion condicionPago'
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
"getter"

^arancel!

cargaDatos: unaEspecialidad
"Permite la carga de datos de una Intervención"

|temp|
codigo:=Contador.
Intervencion acumulador.
descripcion:=(Prompter prompt: 'Ingrese la descripcion'  caption:'Registro > Intervención').
especialidad:=unaEspecialidad.
(temp:=(Prompter prompt: 'Ingrese el arancel' caption: ' Registro > Intervención')).
[((self esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número' caption: ' Registro > Intervención'.
	temp:=(Prompter prompt: 'Ingrese el arancel' caption:' Registro > Intervención').
	((self esFlotante: temp))
].
arancel:=temp asNumber asFloat.

!

codigo
"getter"

^codigo!

descripcion
"getter"

^descripcion!

esFlotante: unNumero
"Permite validar si un número es flotante"

    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp!

especialidad
"getter"

^especialidad!

muestra
"Muestra los datos de una intervención en particular. Utilizado en el menú consultas"

Transcript cr; show: codigo ; tab; tab; show:descripcion ; tab;tab;show:especialidad;tab;tab;show: arancel printString.
(MessageBox notify: 'DESCRIPCIÓN		', descripcion , '
',
'ESPECIALIDAD		', especialidad ,'
','
','ARANCEL			', arancel printString caption: 'Búsqueda de intervenciones > Intervención ',codigo).!

precargaDatos: unCod y: unaDesc y: unaEsp y: unArancel
"Utilizado para pruebas internas. Desestimar"

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

acumulador

Contador:= Contador + 1.
!

esFlotante: unNumero
"Valida si un número es flotante. Este método lo va a usar la clase AltaComplejidad"

    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp!

iniciarContador

Contador:=1! !
!Intervencion class categoriesForMethods!
acumulador!public! !
esFlotante:!public! !
iniciarContador!public! !
!

IntervencionRegistrada guid: (GUID fromString: '{e572bfc1-8db5-432d-a93c-cb49ed4d6a0b}')!
IntervencionRegistrada comment: ''!
!IntervencionRegistrada categoriesForClass!Kernel-Objects! !
!IntervencionRegistrada methodsFor!

cargaDatos: unaFecha y: unPaciente y: unMedico y:unaIntervencion y:unaCondicion
"Se permite la carga de datos para la intervención del paciente, todos los datos son validados"

fecha:=unaFecha.
datosPaciente :=unPaciente.
datosMedico:=unMedico.
datosIntervencion:=unaIntervencion.
condicionPago:= unaCondicion.!

condicionPago
"getter"

^condicionPago!

datosIntervencion
"getter"

^datosIntervencion!

datosMedico
"getter"

^datosMedico!

datosPaciente
"getter"

^datosPaciente!

fecha
"getter"

^fecha!

precargaDatos: unaFecha y: unPaciente y: unMedico y: unaIntervencion y: unaCondicion
"Utilizado para pruebas internas. Desestimar"

fecha:=unaFecha.
paciente:=unPaciente.
medico:=unMedico.
intervencion:=unaIntervencion.
condicionPago:= unaCondicion.! !
!IntervencionRegistrada categoriesForMethods!
cargaDatos:y:y:y:y:!public! !
condicionPago!public! !
datosIntervencion!public! !
datosMedico!public! !
datosPaciente!public! !
fecha!public! !
precargaDatos:y:y:y:y:!public! !
!

Medico guid: (GUID fromString: '{2589d0d7-de77-4739-ac65-7d764c177c02}')!
Medico comment: ''!
!Medico categoriesForClass!Kernel-Objects! !
!Medico methodsFor!

apellido
"getter"

^apellido!

cargaDatos: unaMatricula y: unaEspecialidad y: unaDisponibilidad
"Se cargan los datos de un médico, unaMatricula, unaEspecialidad y unaDisponibilidad vienen validadas"

matricula:=unaMatricula.
nombre:=(Prompter prompt: 'Ingrese el nombre' caption:'Registro > Médico').
apellido:=(Prompter prompt: 'Ingrese el apellido' caption: 'Registro > Médico').
especialidad:=unaEspecialidad.
condicion:=unaDisponibilidad.!

condicion
"getter"

^condicion!

especialidad
"getter"

^ especialidad!

matricula
"getter"

^matricula!

muestra
"Muestra los datos de un médico. Utilizado en el menú consultas."

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
"getter"

^nombre!

precargaDatos: unaMatricula y: unNombre y: unApellido y: unaEspecialidad y: unaCondicion
"Utilizado para pruebas, desestimar"

matricula:= unaMatricula.
nombre:=unNombre.
apellido:=unApellido.
especialidad:=unaEspecialidad.
condicion:=unaCondicion.! !
!Medico categoriesForMethods!
apellido!public! !
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
"getter"

^apellido!

cargaDatos: unDni
"Desde aquí se cargan los datos del paciente. unDNI se pasa como argumento porque viene con datos validados"

|ob temp|
dni:=unDni.
nombre:=(Prompter prompt: 'Ingrese el nombre' caption:'Registro > Paciente').
apellido:=(Prompter prompt: 'Ingrese el apellido' caption: 'Registro > Paciente').
ob:=(MessageBox confirm:'¿Usted posee obra social?' caption: 'Registro > Paciente').
ob ifTrue: [obraSocial:=(Prompter prompt: 'Ingrese el nombre de su obra social' caption: 'Registro > Paciente'). 
(temp:=(Prompter prompt: 'Ingrese el porcentaje de cobertura' caption: 'Registro > Paciente')).
[((self esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número' caption:'Registro > Paciente'.
	temp:=(Prompter prompt: 'Ingrese el porcentaje de cobertura' caption: 'Registro > Paciente').
	((self esFlotante: temp))
].
porcCobertura:=temp asNumber asFloat.
].
ob ifFalse: [obraSocial:='No posee Obra Social'. porcCobertura:=0].
!

dni
"getter"

^dni!

esFlotante: unNumero
"Valida que el numero ingresado sea un entero"

    |temp |
    temp := true.
    [(unNumero asNumber asFloat)]  on: Error do: [:each | temp:= false].
    ^ temp!

muestra
"Muestra los datos relevantes de un determinado paciente. Utilizado en el menú consultas"

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
"getter"

^nombre!

obraSocial
"getter"

^obraSocial!

porcCobertura
"getter"

^porcCobertura!

precargaDatos: unDni y: unNombre y: unApellido y: unaOb y: unPorc
"Utilizado para pruebas, desestimar"

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

calcDescuento: unTotal y: unPorcentaje

"Calcula el descuento usado para la liquidación teniendo en cuenta los parámetros utilzados"

^(unTotal*unPorcentaje)/100

!

consulta
"Menú con las opciones de consulta."
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
"Se ingresa el código y se busca en la colección intervención, si exise, muestra sus datos."

|cod t|

[t isNil and: [(cod = '0') not]] whileTrue:
[
    cod:=Prompter prompt: 'Ingrese el código de la intervención' caption:'Consulta > Intervención'.
    (cod='0') ifTrue: [self consulta] ifFalse: [
        t:= intervencion detect:[:i | i codigo=cod]
        ifNone:[ MessageBox warning: 'Incorrecto. Vuelva a ingresar el código.'. t:= nil.]
    ].
].

(t isNil) ifFalse: [t muestra ]!

consultaMedico
"Se ingresa una matrícula y se busca si existe un objeto con esa matrícula en la colección medico. Si existe, se muestran sus datos."

|mat m|

[m isNil and: [(mat = '0') not]] whileTrue:
[
    mat:=Prompter prompt: 'Ingrese la matrícula del médico' caption:'Consulta > Médico'.
    (mat='0') ifTrue: [self consulta] ifFalse: [
        m:= medico detect:[:i | i matricula=mat ]
        ifNone:[ MessageBox warning: 'Incorrecto. Vuelva a ingresar la matrícula.'. m:= nil.]
    ].
].

m isNil ifFalse: [ m muestra ]
!

consultaPaciente
"Se ingresa un DNI y se lo busca en la colección paciente. Si existe, se muestran sus datos."

|pac p|

[p isNil and: [(pac = '0') not]] whileTrue:
[
    pac:=Prompter prompt: 'Ingrese el DNI del paciente' caption:'Consulta > Paciente'.
    (pac='0') ifTrue: [self consulta] ifFalse: [
        p:= paciente detect:[:i | i dni=pac ]
        ifNone:[ MessageBox warning: 'Incorrecto. Vuelva a ingresar el DNI.'. p:= nil.]
    ].
].

p isNil ifFalse: [ p muestra ]!

esFechaValida: unaFecha
"Valida que el argumento unaFecha, que es un String, pueda ser convertido a una fecha correctamente. Valida además que la fecha no sea del pasado."

    | fechaHoy fecha temp |
    fecha := [Date fromString: unaFecha format: 'MM/DD/yyyy']  on: Error do: [:each | temp:= false].
    (temp=false) ifTrue: [temp:=false] ifFalse:[temp:=true].
    fechaHoy := Date today.
    ^ (temp=true and: [fecha >= fechaHoy]).
	
  !

estadoliquidacion: unDNI
"Recibe como parámetro unDNI y primero valida que exista el paciente. Si existe busca en la colección IntervenciónPaciente los datos pertenecientes a ese paciente con sus intervenciones que no estén pagadas. Si el paciente no registra deudas, se emite el mensaje correspondiente."

	| coleccion2 acumAdic total pac|
	total:= 0.
	acumAdic:=0.
	pac:= (self existeDNI:unDNI).
	coleccion2 := intervencionPaciente select:[:each | each datosPaciente dni = unDNI and:[each condicionPago = false]].
	Transcript clear.
	Transcript show: 'Paciente: ';show: pac nombre;show: ' ';show: pac apellido;show: '  Obra social:  '; show: pac obraSocial; cr.
	Transcript show:'';show: 'Fecha';tab; show: '      Descripcion         ';show: ' Medico     '; show: '  Mat.   '; show: 'Importe'; cr.
        coleccion2 do: [:each |	
            Transcript show: each fecha;show: '    '; show: each datosIntervencion descripcion;show: '   ' ;show: each datosMedico nombre;show:' ';show: each datosMedico apellido;show: '   '; show: each datosMedico matricula;show: ' ';show:' ';show: '$'; print: each datosIntervencion arancel; tab; tab;
                cr.
	((each datosIntervencion) isKindOf: AltaComplejidad) ifTrue: [total:= total + ((each datosIntervencion arancel) * (1+ (AltaComplejidad adicional / 100))). acumAdic:= (((each datosIntervencion arancel) * (1+ (AltaComplejidad adicional / 100)))- each datosIntervencion arancel) + acumAdic ] ifFalse: [total:= total + (each datosIntervencion arancel)]
        ].
	Transcript cr; show: 'Carga por Adicional';tab;tab;tab;show:'$';print: acumAdic rounded; cr.
	Transcript show: 'Total';tab;tab;tab;tab;show:'$';print: total rounded; cr.

	Transcript show: 'Cobertura Obra social';tab;tab;show: '$';print: (self calcDescuento: total y: pac porcCobertura) rounded ;cr.
        Transcript show: 'Neto a pagar       ';tab;tab;tab;show:'$';print: (self netoaPagar: total y: pac porcCobertura) rounded;cr.
	
	(total = 0) ifTrue: [^'Este paciente no registra deudas'] ifFalse:[^(Transcript contents) asString].!

existeCOD: unCOD
"Valida que exista el código de intervención"

|i int|
int:= unCOD.
i:= intervencion detect:[:each | each codigo=int] ifNone:[i:= 'no'.].
(i='no') ifTrue: [^false] ifFalse: [^true ]
!

existeDNI: unDNI
"Valida que exista el código de DNI"
| pac|
pac:= paciente detect:[:i | i dni=unDNI] ifNone:[pac:= 'none'.].
(pac='none') ifTrue: [^false] ifFalse: [^pac ]
!

existeEspecialidad: unaEspecialidad
"Valida que exista una especialidad en la colección intervención. Muy útil para validaciones posteriores."

|e|
e:= intervencion detect:[:i | i especialidad=unaEspecialidad] ifNone:[e:= 'no'.].
(e='no') ifTrue: [^false] ifFalse: [^true ]!

existeMatricula: unaMatricula
"Valida que exista la matrícula enviada en el argumento en la colección medico"
| m med|
med:= unaMatricula.
m:= medico detect:[:i | i matricula=med] ifNone:[m:= 'no'.].
(m='no') ifTrue: [^false] ifFalse: [^true ]!

inicio
"Se inician las colecciones y se ejecuta el menú"
	paciente := OrderedCollection new.
	medico := OrderedCollection new.
	intervencion := OrderedCollection new.
	intervencionPaciente:= OrderedCollection new.
	Intervencion iniciarContador.
!

intervencionesDisponibles: unaEspecialidad
"Lista de intervenciones disponibles teniendo en cuenta la especialidad seleccionada."

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
                print: each codigo; tab;tab;
		show: each descripcion; tab;tab;tab;
		show: each arancel printString; tab;tab;
                cr.
        ].
	temp:= (Transcript contents) asString.
	^temp
    ]!

liquidacion
"Permite que se ingrese el DNI del usuario del que se quiera obtener la liquidación, valida que exista y muestra la liquidación invocando al método estadoliquidacion"

|coleccion2 rta dni|


(paciente isEmpty or: [intervencion isEmpty or: [medico isEmpty]]) ifTrue: [MessageBox errorMsg: 'BASE DE DATOS VACIA.' caption: 'Error del sistema'] ifFalse: [
rta:=1.
dni:=(Prompter prompt: 'Ingrese el DNI del paciente con intervenciones registradas').
[rta = 1] whileTrue:[
	coleccion2 := intervencionPaciente select: [:each | (each datosPaciente dni) = dni and:[each condicionPago = false]].
	(coleccion2 isEmpty) ifTrue: [
	(MessageBox warning: 'No existe una intervencion pendiente de pago registrado con ese DNI. Intente nuevamente').
] ifFalse: [
	MessageBox notify: (self estadoliquidacion: dni) caption: 'LIQUIDACIÓN'.
].
	dni:=(Prompter prompt: 'Ingrese otro DNI o 0 para salir').
	(dni = '0') ifTrue:[ rta:=0].
].

]
!

medicosDisponibles: unaEspecialidad y: unaOpcion
"Muestra una lista de médicos disponibles teniendo en cuenta la especialidad. Muy importante para validaciones y para el registro de intervenciones de pacientes. De acuerdo a la opción, mostrará una lista o un booleano indicando si existen médicos para esa especialidad o no."

|coleccion1 coleccion2 temp temp2|
coleccion1 := medico select: [:each | each condicion].
coleccion2:= coleccion1 select: [:each | each especialidad=unaEspecialidad].
(coleccion2 isEmpty) 
    ifTrue:[(unaOpcion =1) ifTrue: [MessageBox notify: 'No hay médicos disponibles.'.].  temp2:=false.]
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
"Menú desde el cual se pueden registrar intervenciones para pacientes y acceder a la liquidación. Para realizar altas se deberá ingresar al menú admin, introduciendo /admin como opción"

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
"Menú que permite el alta de todas las colecciones críticas"

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
"Método que calcula el monto neto a pagar. Se utiliza en estadoLiquidacion:"
^(unTotal - (self calcDescuento: unTotal y: unPorcentaje)) !

registrarIntervencion
"Se registra una intervención validando además que haya médicos disponibles para la especialidad correspondiente."

|rta rta2 t especialidad|

rta:= true.

(intervencion isEmpty) ifTrue: [AltaComplejidad cargaAdicional. MessageBox notify: 'Adicional agregado con éxito' .].
[rta] whileTrue: [
        especialidad:=(Prompter prompt: 'Ingrese la especialidad').
	((self medicosDisponibles: especialidad y: 2)=false) ifTrue: [MessageBox warning: 'No hay médicos disponibles para esa especialidad.' caption:'Menú administrador > Registro > Intervención'.]
	ifFalse:[
	rta2 := MessageBox confirm: '¿Es una intervencion de alta complejidad?' caption:'Menú administrador > Registro > Intervención'.
        t := rta2
            ifTrue: [AltaComplejidad new]
            ifFalse: [Intervencion new].
        t cargaDatos: especialidad.
        intervencion add: t.
    ].
	rta:= MessageBox confirm: '¿Desea ingresar otra intervencion?' caption:'Menú administrador > Registro > Intervención'
].!

registrarIntervencionPaciente
"Se registra una intervención de paciente, validando todos los datos correspondientes."

|rta p fecha inter matricula pac espe|

rta:= true.
(paciente isEmpty or: [intervencion isEmpty or: [medico isEmpty]]) ifTrue: [MessageBox errorMsg: 'BASE DE DATOS VACIA.' caption: 'Error del sistema'] ifFalse: [
[rta] whileTrue: [
    fecha := (Prompter prompt: 'Ingrese una fecha. (MM/DD/YYYY)' caption:'Menú administrador > Registro > Intervención de paciente').
    (self esFechaValida: fecha) ifFalse: [
        MessageBox warning: 'Fecha inválida. Vuelva a intentarlo.' caption:'Menú administrador > Registro > Intervención de paciente'.
    ] ifTrue: [
	pac:= (Prompter prompt: 'Ingrese el DNI del paciente' caption:'Menú administrador > Registro > Intervención de paciente').
	[(self existeDNI: pac) ~= false] whileFalse: [
	      pac:= MessageBox errorMsg: 'El documento ingresado no coincide con nuestros registros. Vuelva a intentarlo.' caption:'Menú administrador > Registro > Intervención de paciente'.
	      pac:= (Prompter prompt: 'Ingrese el DNI del paciente' caption:'Menú administrador > Registro > Intervención de paciente').].
	pac:= (self existeDNI: pac).

	espe:= Prompter prompt: 'Ingrese la especialidad.' caption:'Menú administrador > Registro > Intervención de paciente'.
	[self existeEspecialidad: espe ] whileFalse: [
		MessageBox warning: ('Los datos ingresados no coinciden con nuestros registros. Vuelva a intentarlo.') caption:'Menú administrador > Registro > Intervención de paciente'.
		espe:= Prompter prompt:'Ingrese la especialidad.' caption:'Menú administrador > Registro > Intervención de paciente'.
		self existeEspecialidad: espe.
	].
	MessageBox notify:(self medicosDisponibles: espe y: 1).
	matricula:= Prompter prompt: 'Ingrese la matrícula del profesional' caption:'Menú administrador > Registro > Intervención de paciente'.
	[(self validarMedico: matricula y: espe) ~= false] whileFalse: [
		MessageBox warning:('La matrícula ingresada no coincide con nuestros registros. Vuelva a intentarlo.') caption:'Menú administrador > Registro > Intervención de paciente'.
		MessageBox notify:(self medicosDisponibles: espe y: 1).
		matricula:= Prompter prompt: 'Ingrese la matrícula del profesional.' caption:'Menú administrador > Registro > Intervención de paciente'.].
	matricula:= (self validarMedico: matricula y: espe).

	MessageBox notify:(self intervencionesDisponibles: espe).
	inter:= (Prompter prompt: 'Ingrese el código de intervención' caption:'Menú administrador > Registro > Intervención de paciente') asNumber.
	[(self validarIntervencion: inter y: espe) ~= false] whileFalse: [
		MessageBox warning:('El código de intervención ingresado no coincide con nuestros registros. Vuelva a intentarlo.') caption:'Menú administrador > Registro > Intervención de paciente'.
		MessageBox notify:(self intervencionesDisponibles: espe).
		inter:= (Prompter prompt: 'Ingrese el código de intervención' caption:'Menú administrador > Registro > Intervención de paciente') asNumber].
	inter:= (self validarIntervencion: inter y: espe).
	
	"Agregar en pac matricula y inter, el objeto completo."
        p:= IntervencionRegistrada new.
        p cargaDatos: fecha y: pac y: matricula y: inter y: (MessageBox confirm: '¿Está pagada?' ).
        intervencionPaciente add: p.
        rta:= MessageBox confirm: '¿Desea registrar otra intervención?' caption:'Menú administrador > Registro > Intervención de paciente'
    ]]].
!

registrarMedico
"Se lleva a cabo el registro de médicos, validando además que siempre haya un médico disponible por cada especialidad"

|rta m matricula especialidad disponibilidad|

rta:= true.

[rta] whileTrue: [
    matricula := (Prompter prompt: 'Ingrese la matrícula' caption:'Menú administrador > Registro > Médico').
    (self existeMatricula: matricula) ifTrue: [
        MessageBox warning: 'La matrícula ya existe. Por favor, ingrese otra.' caption:'Menú administrador > Registro > Médico'.
    ] ifFalse: [
	especialidad := (Prompter prompt: 'Ingrese la especialidad' caption:'Menú administrador > Registro > Médico').
	disponibilidad := (MessageBox confirm: '¿Está disponible?' caption:'Menú administrador > Registro > Médico').
	(((self medicosDisponibles: especialidad y: 2)=false) and:[disponibilidad=false]) ifTrue: [
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
"Se registra un paciente validando todos los datos correspondientes"

|rta p dni|
	
rta:= true.

[rta] whileTrue: [
    dni := (Prompter prompt: 'Ingrese DNI' caption:'Menú administrador > Registro > Paciente').
    ((self existeDNI: dni) ~= false) ifTrue: [
        MessageBox warning: 'El DNI ya existe. Por favor, ingrese otro.' caption:'Menú administrador > Registro > Paciente'.
    ] ifFalse: [
        p:= Paciente new.
        p cargaDatos: dni .
        paciente add: p.
        rta:= MessageBox confirm: 'Desea ingresar otro paciente?' caption:'Menú administrador > Registro > Paciente'
    ]].!

validarIntervencion: unCodigo y: unaEspec
"Valida que exista una intervención con el código y especialidad mandados como parámetros"
| Inter |
Inter:= intervencion detect:[:i | i codigo=unCodigo and: [i especialidad=unaEspec] ] ifNone:[Inter:= 'none'.].
(Inter = 'none') ifTrue: [^false] ifFalse: [^Inter].!

validarMedico: unaMatricula y: unaEspecialidad 
"Valida que exista un médico en la colección teniendo en cuenta su matrícula, especialidad y condición"

| Med |
Med:= medico detect:[:i | i matricula=unaMatricula and: [i especialidad=unaEspecialidad and: [i condicion=true] ] ] ifNone:[Med:= 'none'.].
(Med='none') ifTrue: [^false] ifFalse: [^Med ]! !
!Sanatorio categoriesForMethods!
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
"getter"

^Adicional!

cargaAdicional
"Permite ingresar el adicional de las Intervenciones de alta complejidad"

|temp|
(temp:=(Prompter prompt: 'Ingrese el adicional' caption:'Intervenciones > Alta complejidad')).
[((super esFlotante: temp)=false)] whileTrue: [
	MessageBox errorMsg: 'Debe ingresar un número' caption:'Intervenciones > Alta complejidad'.
	temp:=(Prompter prompt: 'Ingrese el adicional' caption:'Intervenciones > Alta complejidad').
	((super esFlotante: temp)).
].
Adicional:=temp asNumber asFloat.!

precargaAdicional: unAdic
"Utilizado para pruebas internas. Desestimar"

Adicional:=unAdic.! !
!AltaComplejidad class categoriesForMethods!
adicional!public! !
cargaAdicional!public! !
precargaAdicional:!public! !
!

"Binary Globals"!

