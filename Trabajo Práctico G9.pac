﻿| package |
package := Package name: 'Trabajo Práctico G9'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AltaComplejidad;
	add: #Intervencion;
	add: #Medico;
	add: #Paciente;
	add: #Sanatorio;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'C:\Users\Fringe\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'C:\Users\Fringe\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'C:\Users\Fringe\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Intervencion
	instanceVariableNames: 'fecha medico intervencion condicionPago codigo descripcion especialidad arancel'
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
	instanceVariableNames: 'medico paciente intervencion'
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

cargaDatos

^ MessageBox notify: 'CargaDatos'!

mostrarCondicion

^ MessageBox notify: 'Mostrar condicion'! !
!Intervencion categoriesForMethods!
cargaDatos!public! !
mostrarCondicion!public! !
!

Medico guid: (GUID fromString: '{2589d0d7-de77-4739-ac65-7d764c177c02}')!
Medico comment: ''!
!Medico categoriesForClass!Kernel-Objects! !
!Medico methodsFor!

cargaDatos: unaMatricula

matricula:=unaMatricula.
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
especialidad:=(Prompter prompt: 'Ingrese la especialidad').
condicion:=(MessageBox confirm:'¿Está disponible?').!

condicion

^ MessageBox notify: 'Condición'!

especialidad

^ MessageBox notify: 'Especialidad'!

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

precargaDatos: unaMatricula y: unNombre y: unApellido y: unaEspecialidad y: unaCondicion
matricula:= unaMatricula.
nombre:=unNombre.
apellido:=unApellido.
especialidad:=unaEspecialidad.
condicion:=unaCondicion.! !
!Medico categoriesForMethods!
cargaDatos:!public! !
condicion!public! !
especialidad!public! !
matricula!public! !
muestra!public! !
precargaDatos:y:y:y:y:!public! !
!

Paciente guid: (GUID fromString: '{6d238f25-d0b2-42a7-9548-b835c9b80ed3}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

apellido
^apellido!

cargaDatos: unDni

|ob|
dni:=unDni.
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
ob:=(MessageBox confirm:'¿Usted posee obra social?').
ob ifTrue: [obraSocial:=(Prompter prompt: 'Ingrese el nombre de su obra social'). porcCobertura:=(Prompter prompt: 'Ingrese el porcentaje de cobertura') asNumber asFloat].
ob ifFalse: [obraSocial:='No posee Obra Social'. porcCobertura:=0].
!

dni
^dni!

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
muestra!public! !
nombre!public! !
precargaDatos:y:y:y:y:!public! !
!

Sanatorio guid: (GUID fromString: '{bb5d2598-1dbf-4024-8299-8a84559782e8}')!
Sanatorio comment: ''!
!Sanatorio categoriesForClass!Kernel-Objects! !
!Sanatorio methodsFor!

consulta
|op|
op:='5'.
[op='0'] whileFalse: [(MessageBox notify: '1 - Buscar pacientes
2 - Buscar médicos
3. Buscar Intervenciones
0 - Volver al menú' caption: 'CONSULTA').
op:=(Prompter prompt: 'Ingrese una opción').
(op='1') ifTrue: [self consultaPaciente].
(op='2') ifTrue: [self consultaMedico].
(op='3') ifTrue: [self consultaIntervencion ].
(((op='1' or: [op='2']) or: [op='0']) or:[op='0']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')].
]
!

consultaIntervencion

^ MessageBox notify: 'Int'!

consultaMedico

|mat m|

[m isNil] whileTrue:[
mat:=Prompter prompt: 'Ingrese la matrícula del profesional'.
m:= medico detect:[:i | i matricula=mat]
ifNone:[ MessageBox notify: 'Incorrecto. Vuelva a ingresar legajo o escriba SALIR para regresar al menú.'. m:= nil. ((mat='SALIR') ifTrue: [m:='3']) ]].

(m isNil) ifFalse: [m muestra].

 !

consultaPaciente

|pac p|

[p isNil] whileTrue:[
pac:=Prompter prompt: 'Ingrese el DNI del paciente'.
p:= paciente detect:[:i | i dni=pac ]
ifNone:[ MessageBox notify: 'Incorrecto. Vuelva a ingresar el DNI o escriba SALIR para regresar al menú.'. p:= nil. ((pac='SALIR') ifTrue: [p:='3'])]].

(p isNil) ifFalse: [p muestra].!

existeDNI: unDNI
| p pac|
pac:= unDNI.
p:= paciente detect:[:i | i dni=pac] ifNone:[p:= 'no'.].
(p='no') ifTrue: [^false] ifFalse: [^true ]
!

existeMatricula: unaMatricula
| m med|
med:= unaMatricula.
m:= medico detect:[:i | i matricula=med] ifNone:[m:= 'no'.].
(m='no') ifTrue: [^false] ifFalse: [^true ]!

inicio

paciente:=OrderedCollection new.
medico:=OrderedCollection new.
intervencion:=OrderedCollection new.


paciente add: (( Paciente new)
    precargaDatos: '01' y: 'Joel' y: 'Marchesa' y: 'Swiss Medical' y: 10
    yourself).

paciente add: (( Paciente new)
    precargaDatos: '02' y: 'Tomas' y: 'Messa' y: 'No posee Obra Social' y: 0
    yourself).
paciente add: (( Paciente new)
    precargaDatos: '01' y: 'Joel' y: 'Marchesa' y: 'Swiss Medical' y: 10
    yourself).

paciente add: (( Paciente new)
    precargaDatos: '03' y: 'Ulises' y: 'Gutiérrez' y: 'Obra Social: OSDE' y: 15
    yourself).

medico add: (( Medico new)
    precargaDatos: '01' y: 'Ramón' y: 'Pascual' y: 'Cardiología' y: true
    yourself).

medico add: (( Medico new)
    precargaDatos: '02' y: 'Valentín' y: 'Vidente' y: 'Oftalmología' y: true
    yourself).

medico add: (( Medico new)
    precargaDatos: '03' y: 'Roberto' y: 'Neuross' y: 'Neurología' y: false
    yourself).!

liquidacion

^ MessageBox notify: 'liquidacion'!

listar: coleccion
Transcript cr;show: 'NOMBRE';tab;show:'LEGAJO';tab;show:'NOTA';cr.
coleccion do: [:each | each muestra ]!

menu
|op|

op:='5'.
[op='0'] whileFalse: [MessageBox notify: '1 - Registrar Intervencion 
2 - Mostrar liquidacion
3. Consultas
0 - Salir' caption: 'MENU DE OPCIONES'.
op:=(Prompter prompt: 'Ingrese una opcion').
(op='1') ifTrue: [self registrarIntervencion].
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
((op='1' or: [op='2']) or: [op='0']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')]
]

!

registrarIntervencion

^ MessageBox notify: 'Registrar Intervencion'!

registrarMedico

|rta m matricula|

rta:= true.

[rta] whileTrue: [
    matricula := (Prompter prompt: 'Ingrese la matrícula').
    (self existeMatricula: matricula) ifTrue: [
        MessageBox notify: 'La matrícula ya existe. Por favor, ingrese otra.'.
    ] ifFalse: [
        m:= Medico new.
        m cargaDatos: matricula .
        medico add: m.
        rta:= MessageBox confirm: 'Desea ingresar otro paciente?'
    ]].
!

registrarPaciente
|rta p dni|

rta:= true.

[rta] whileTrue: [
    dni := (Prompter prompt: 'Ingrese DNI').
    (self existeDNI: dni) ifTrue: [
        MessageBox notify: 'El DNI ya existe. Por favor, ingrese otro.'.
    ] ifFalse: [
        p:= Paciente new.
        p cargaDatos: dni .
        paciente add: p.
        rta:= MessageBox confirm: 'Desea ingresar otro paciente?'
    ]].!

validarMedico

^ MessageBox notify: 'valMedico'!

validarPaciente
! !
!Sanatorio categoriesForMethods!
consulta!public! !
consultaIntervencion!public! !
consultaMedico!public! !
consultaPaciente!public! !
existeDNI:!public! !
existeMatricula:!public! !
inicio!public! !
liquidacion!public! !
listar:!public! !
menu!public! !
menuAdmin!public! !
registrarIntervencion!public! !
registrarMedico!public! !
registrarPaciente!public! !
validarMedico!public! !
validarPaciente!public! !
!

AltaComplejidad guid: (GUID fromString: '{a632a6b3-501a-4a70-abee-034483a530bc}')!
AltaComplejidad comment: ''!
!AltaComplejidad categoriesForClass!Kernel-Objects! !
!AltaComplejidad methodsFor!

calcularArancel

^ MessageBox notify: 'Calcular arancel'! !
!AltaComplejidad categoriesForMethods!
calcularArancel!public! !
!

!AltaComplejidad class methodsFor!

cargaAdicional

^ MessageBox notify: 'CargaAdicional'! !
!AltaComplejidad class categoriesForMethods!
cargaAdicional!public! !
!

"Binary Globals"!

