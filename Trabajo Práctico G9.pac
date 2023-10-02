| package |
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

cargaDatos

nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
matricula= (Prompter prompt: 'Ingrese su matrícula').
especialidad:=(Prompter prompt: 'Ingrese la especialidad').
condicion:=(MessageBox confirm:'¿Está disponible??').!

condicion

^ MessageBox notify: 'Condición'!

especialidad

^ MessageBox notify: 'Especialidad'! !
!Medico categoriesForMethods!
cargaDatos!public! !
condicion!public! !
especialidad!public! !
!

Paciente guid: (GUID fromString: '{6d238f25-d0b2-42a7-9548-b835c9b80ed3}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

cargaDatos

dni:= (Prompter prompt: 'Ingrese el DNI').
nombre:=(Prompter prompt: 'Ingrese el nombre').
apellido:=(Prompter prompt: 'Ingrese el apellido').
obraSocial:=(MessageBox confirm:'Usted posee obra social?').
obraSocial ifTrue: [porcCobertura:=(Prompter prompt: 'Ingrese el porcentaje de cobertura')].
obraSocial ifFalse: [porcCobertura:=0].
! !
!Paciente categoriesForMethods!
cargaDatos!public! !
!

Sanatorio guid: (GUID fromString: '{bb5d2598-1dbf-4024-8299-8a84559782e8}')!
Sanatorio comment: ''!
!Sanatorio categoriesForClass!Kernel-Objects! !
!Sanatorio methodsFor!

inicio

paciente:=OrderedCollection new.
medico:=OrderedCollection new.
intervencion:=OrderedCollection new.!

liquidacion

^ MessageBox notify: 'liquidacion'!

menu
|op|

op:='5'.
[op='0'] whileFalse: [MessageBox notify: '1 - Registrar Intervencion 
2 - Mostrar liquidacion
0 - Salir' caption: 'MENU DE OPCIONES'.
op:=(Prompter prompt: 'Ingrese una opcion').
(op='1') ifTrue: [self registrarIntervencion].
(op='2') ifTrue: [self liquidacion].
(op= '/admin') ifTrue: [self menuAdmin].
(((op='1' or: [op='2']) or: [op='/admin']) or: [op='0']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')]
]

!

menuAdmin
|op|

op:='4'.
[op='0'] whileFalse: [MessageBox notify: '1 - Registrar paciente
2 - Registrar médico
3 - Registrar intervención
0 - Salir' caption: 'PANEL DE ADMINISTRADOR'.
op:=(Prompter prompt: 'Ingrese una opción:').
(op='1') ifTrue: [self registrarPaciente].
(op= '2') ifTrue: [self registrarMedico].
((op='1' or: [op='2']) or: [op='0']) ifFalse: [op:=(Prompter prompt: 'Opcion invalida. Ingrese otra.')]
]

!

registrarIntervencion

^ MessageBox notify: 'Registrar Intervencion'!

registrarMedico

|rta m|

rta:= true.

[rta] whileTrue: [m:=Medico new.
m cargaDatos.
medico add: m.
rta:= MessageBox confirm: '¿Desea ingresar otro médico?'].
!

registrarPaciente

|rta p|

rta:= true.

[rta] whileTrue: [p:=Paciente new.
p cargaDatos.
paciente add: p.
rta:= MessageBox confirm: 'Desea ingresar otro paciente?'].

!

validarMedico

^ MessageBox notify: 'valMedico'! !
!Sanatorio categoriesForMethods!
inicio!public! !
liquidacion!public! !
menu!public! !
menuAdmin!public! !
registrarIntervencion!public! !
registrarMedico!public! !
registrarPaciente!public! !
validarMedico!public! !
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

