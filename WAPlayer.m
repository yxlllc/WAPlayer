(* ::Package:: *)

BeginPackage["WAPlayer`"]

WAPlayer::usage=
"WAPlayer[address_String]:  Play an audio file in the disk
WAPlayer[sound_Sound]:  Play an Sound object in Wolfram Language
WAPlayer[audio_Audio]:  Play an Audio object in Wolfram Language
WAPlayer[args___]:  It is equivalent to WAPlayer@Audio[args]"

InitializeSynthesizer::usage=
"InitializeSynthesizer[ ]:  Initialize all the instrument of midi synthesizer"

LoadSoundBank::usage=
"LoadSoundBank[arg___]:  Load all the instrument found in FileNames[arg] for the midi synthesizer"

MMASoundFonts::usage=
"MMASoundFonts[ ]:  Find out the folder address where Mathematica stores its own sf2 sound banks."

Begin["`Private`"]

InitializeSynthesizer[]:=
(
	If[!ValueQ[$Synthesizer],
		JLink`InstallJava[];
		JLink`LoadJavaClass["javax.sound.midi.MidiSystem"]];
	
	If[JLink`JavaObjectQ[$Synthesizer],
		$Synthesizer@close[];
		JLink`ReleaseJavaObject[$Synthesizer]];

	$Synthesizer=javax`sound`midi`MidiSystem`getSynthesizer[];
	$Synthesizer@open[]
)

LoadSoundBank[arg___]:=
	Module[{object},
		Do[
			object=javax`sound`midi`MidiSystem`getSoundbank@JLink`JavaNew["java.io.File",f];
			$Synthesizer@loadAllInstruments[object];
			JLink`ReleaseJavaObject[object],
			{f,FileNames[arg]}
			]
		]

MMASoundFonts[]:=
	Module[{paclets},
		PacletManager`Package`getPacletWithProgress[ "SoundFontIndex"];
		paclets = DeleteDuplicates@Values@Get@PacletManager`PacletResource["SoundFontIndex", "index.m"];
		PacletManager`Package`getPacletWithProgress/@paclets;
		PacletManager`Package`PgetLocation@First@PacletManager`PacletFind[#]&/@paclets
		]


buttonstyle=
	<|
		"Play"->Graphics[{RGBColor[0,0.478,0.851],Triangle[{{-(Sqrt[2]/4),Sqrt[2]/2},{Sqrt[2]/4,0},{-(Sqrt[2]/4),-(Sqrt[2]/2)}}]},PlotRange->{{-1,1},{-1,1}},Background->None],
		"Pause"->Graphics[{RGBColor[0,0.478,0.851],Rectangle[{-(1/2),-(Sqrt[2]/2)},{-(1/6),Sqrt[2]/2}],Rectangle[{1/6,-(Sqrt[2]/2)},{1/2,Sqrt[2]/2}]},PlotRange->{{-1,1},{-1,1}},Background->None],
		"Stop"->Graphics[{RGBColor[0,0.478,0.851],Rectangle[{-(1/2),-(1/2)},{1/2,1/2}]},PlotRange->{{-1,1},{-1,1}},Background->None]
	|>;

buttonoption={Appearance->"Palette",FrameMargins->0,ImageSize->{25,25}};

MixPlayer[audio_,bytes_]:=
	DynamicModule[{sequencer,midistream,options,id,duration},
	
		If[bytes=!=None,
			If[!JLink`JavaObjectQ[$Synthesizer],
				InitializeSynthesizer[]];
			sequencer=javax`sound`midi`MidiSystem`getSequencer[False];
			sequencer@open[];
			sequencer@getTransmitter[]@setReceiver @ $Synthesizer@getReceiver[];
			midistream=JLink`JavaNew["java.io.ByteArrayInputStream",bytes];
			sequencer@setSequence @midistream];
	
		id=Last@
		If[audio=!=None,
			options=Options[audio];
			Audio`AudioStreamInternals`createAudioStream[
				audio,
				Lookup[options,AudioOutputDevice,Automatic]/.Automatic->$DefaultAudioOutputDevice,
				Lookup[options,SoundVolume,1],
				Round[0.2Audio`Utilities`AudioSampleRate[audio]],
				3,
				4,
				"Internal",
				None,
				Lookup[options,AudioChannelAssignment,Automatic],
				False],
				
			Audio`AudioStreamInternals`createAudioStream[
				Audio[ConstantArray[0.,Ceiling[sequencer@getMicrosecondLength[]/10^4]],SampleRate->100],
				$DefaultAudioOutputDevice,
				1,
				20,
				3,
				4,
				"Internal",
				None,
				Automatic,
				False]
			];
			
		duration=Audio`AudioStreamInternalsDump`$audioStreams["Internal",id,"Duration"];

	Row[{
		Button[
			buttonstyle["Play"],
		
			If[sequencer@getMicrosecondPosition[]>=sequencer@getMicrosecondLength[],
				sequencer@setMicrosecondPosition[0]];
			sequencer@start[];
			Audio`AudioStreamInternals`playAudioStream["Internal",id],
		
			buttonoption],
		
		Button[
			buttonstyle["Pause"],
			
			sequencer@stop[];
			Audio`AudioStreamInternals`pauseAudioStream["Internal",id];
			sequencer@setMicrosecondPosition@Floor[10^6 Audio`AudioStreamInternalsDump`$audioStreams["Internal",id,"Position"]],
			
			buttonoption],
			
		Button[
			buttonstyle["Stop"],
		
			sequencer@stop[];
			Audio`AudioStreamInternals`stopAudioStream["Internal",id];
			sequencer@setMicrosecondPosition[0],
			
			buttonoption],
			
		Spacer[10],
		
		Slider[
			Dynamic[
				Min[Audio`AudioStreamInternalsDump`$audioStreams["Internal",id,"Position"],duration],
				
					(sequencer@setMicrosecondPosition[Floor[10^6 #]];
						If[!sequencer@isRunning[]&&Audio`AudioStreamInternalsDump`$audioStreams["Internal",id,"Status"]==="Playing",
							sequencer@start[]];
						Quiet@Audio`AudioStreamInternalsDump`setAudioStreamPosition["Internal",id,#])&
				],
				
			{0,duration},ImageSize->{300,20},Appearance->"Labeled"]
			
		}],
		
	Deinitialization:>
		(Audio`AudioStreamInternals`removeAudioStream["Internal",id];
			sequencer@close[];
			JLink`ReleaseJavaObject[sequencer];
			JLink`ReleaseJavaObject[midistream])

	]


WAPlayer[address_String/;StringEndsQ[address,".mid"]]:=
MixPlayer[
	None,
	If[StringStartsQ[address,"http"|"https"~~"://"],
		URLRead[address,"BodyBytes"],
		BinaryReadList[address]]
		]

WAPlayer[midisequence_Sound`MIDISequence]:=MixPlayer[None,Sound`MIDISequenceToBytes@midisequence]

WAPlayer[sound_Sound]:=
	MixPlayer[
		If[#1===$Failed||#1===None,None,Audio[#1]],
		If[#2===$Failed||#2===None,None,Sound`MIDISequenceToBytes[#2]]
	]&@@Sound`NormalizeSound@sound

WAPlayer[args___]:=MixPlayer[Audio[args],None]

End[ ]

EndPackage[ ]
