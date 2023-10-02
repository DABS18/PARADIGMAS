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
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'C:\Users\IPP\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box').

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

^ MessageBox notify: 'CargaDatos'!

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

^ MessageBox notify: 'CargaDatos'! !
!Paciente categoriesForMethods!
cargaDatos!public! !
!

Sanatorio guid: (GUID fromString: '{bb5d2598-1dbf-4024-8299-8a84559782e8}')!
Sanatorio comment: ''!
!Sanatorio categoriesForClass!Kernel-Objects! !
!Sanatorio methodsFor!

inicio

^ MessageBox notify: 'Inicio'!

liquidacion

^ MessageBox notify: 'liquidacion'!

menu

^ MessageBox notify: 'Menú'!

registrarIntervencion

^ MessageBox notify: 'Registrar Intervencion'!

registrarPaciente

^ MessageBox notify: 'registrar Paciente'!

validarMedico

^ MessageBox notify: 'valMedico'! !
!Sanatorio categoriesForMethods!
inicio!public! !
liquidacion!public! !
menu!public! !
registrarIntervencion!public! !
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

