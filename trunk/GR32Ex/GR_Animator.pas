(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GR_Animator
 *
 * The Initial Developer of the Original Code is Riceball LEE
 * Portions created by Riceball LEE are Copyright (C) 2008
 * All Rights Reserved.
 *
 * Contributor(s):
 *  Idea come from FlexSDK
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Animator;

{$I Setting.inc}

interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows,
  SysUtils, Classes
  , Graphics
  , GR32
  , GR32_Layers
  , GR32_Transforms
  , GR32_Filters
  //, GR_Graphics
  //, GR_FilterEx
  //, GR_GraphUtils
  //, PNGImage
  //, GR32_PNG
  ;

type
  TGRAnimator = class

  TGRCustomAnimator = class
  protected
    FDuration: Integer;

    {
     *  Plays the effect instance on the target.
     *  Call the <code>startEffect()</code> method instead
     *  to make an effect start playing on an EffectInstance.
     * 
     *  <p>In a subclass of EffectInstance, you must override this method. 
     *  The override must call the <code>super.play()</code> method 
     *  so that an <code>effectStart</code> event is dispatched
     *  from the target.</p>
     }
    procedure Play(const aPlayReversedFromEnd: Boolean = false); virtual; abstract;

    
    {
     *  Pauses the effect until you call the <code>resume()</code> method.
     }
    procedure Pause();
    
    {
     *  Stops the effect, leaving the target in its current state.
     *  This method is invoked by a call
     *  to the <code>Effect.stop()</code> method. 
     *  As part of its implementation, it calls
     *  the <code>finishEffect()</code> method.
     *
     *  <p>The effect instance dispatches an <code>OnAniEnd</code> event
     *  when you call this method as part of ending the effect.</p>
     }
    procedure Stop();

    {
     *  Resumes the effect after it has been paused 
     *  by a call to the <code>pause()</code> method. 
     }
    procedure Resume();
    
    {
     *  Plays the effect in reverse, starting from
     *  the current position of the effect.
     }
    procedure Reverse();

    {
     *  Interrupts an effect instance that is currently playing,
     *  and jumps immediately to the end of the effect.
     *  This method is invoked by a call
     *  to the <code>Effect.end()</code> method. 
     *  As part of its implementation, it calls
     *  the <code>finishEffect()</code> method.
     *
     *  <p>The effect instance dispatches an <code>OnAniEnd</code> event
     *  when you call this method as part of ending the effect.</p>
     *
     *  <p>In a subclass of EffectInstance,
     *  you can optionally override this method
     *  As part of your override, you should call
     *  the <code>super.end()</code> method
     *  from the end of your override, after your logic.</p>
     *
     }
    procedure GotoEnd();

    {
       *  Duration of the animation in milliseconds. 
       *
       *  <p>In a Parallel or Sequence effect, the <code>duration</code>
       *  property sets the duration of each effect.
       *  For example, if a Sequence effect has its <code>duration</code>
       *  property set to 3000, each effect in the Sequence takes 3000 ms
       *  to play.</p>
       *
       *  <p>For a repeated effect, the <code>duration</code> property
       *  specifies  the duration of a single instance of the animation. 
       *  Therefore, if an animation has a <code>duration</code> property
       *  set to 2000, and a <code>RepeatCount</code> property set to 3, 
       *  the animation takes a total of 6000 ms (6 seconds) to play.</p>
       *
       *  @default 500
    }
    property Duration: Integer read FDuration;
    {
     *  The UIComponent object to which this animation is applied.
     *
     }
    property Target: TObject read GetTarget write SetTarget;

    {
     *  If <code>true</code>, blocks all background processing
     *  while the animation is playing.
     *  Background processing includes measurement, layout,
     *  and processing responses that have arrived from the server.
     *  
     *  @default false
     }
    property SuspendBackgroundProcessing: Boolean;

    {
       Current position in time of the animation.
       This property has a value between 0 and the actual duration 
       (which includes the value of the <code>StartDelay</code>, 
       <code>RepeatCount</code>, and <code>RepeatDelay</code> properties).
     }
    property PlayheadTime: Integer read GetPlayheadTime;
    {
       Number of times to repeat the effect.
       Possible values are any integer greater than or equal to 0.
       
       @default 1
       @see mx.effects.Effect#repeatCount
     }
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
    {
     *  Amount of time, in milliseconds,
     *  to wait before repeating the animation.
     *  
     *  @default 0
     *  @see TGRAnimators.RepeatDelay
     }
    property RepeatDelay: Integer;
    {
     *  Amount of time, in milliseconds,
     *  to wait before starting the animation.
     *  Possible values are any int greater than or equal to 0.
     *  If the animation is repeated by using the <code>repeatCount</code>
     *  property, the <code>startDelay</code> property is applied 
     *  only the first time the effect is played.
     *
     *  @default 0
     }
    property StartDelay: Integer;

    {
     *  The Trigger, if any, which triggered the playing of the animation.
     *  This property is useful when an animation is assigned to 
     *  multiple triggers.
     * 
     *  <p>If the animation was played programmatically by a call to the 
     *  <code>play()</code> method, rather than being triggered by an event,
     *  this property is <code>null</code>.</p>
     *  if it is the AnimatorInstance then Trigger only one if any.
     }
    property Trigger: TStrings;
  end;

  { Summary: the abstract animator. one target one Animator. }
  {
   *  The TGRAnimatorInstance interface represents an instance of an animation
   *  playing on a target.
   *  Each target has a separate animation instance associated with it.
   *  An effect instance's lifetime is transitory.
   *  An instance is created when the animation is played on a target
   *  and is destroyed when the animation has finished playing. 
   *  If there are multiple animation playing on a target at the same time 
   *  (for example, a Parallel effect), there is a separate animator instance
   *  for each animation.
   * 
   *  <p>Animation developers must create an instance class
   *  for their custom animations.</p>
   *
   *  @see TGRAnimator
  }
  TGRAnimatorInstance = class(TGRCustomAnimator)
  protected
//    FTrigger: string;
    {
     *  This method is called if the effect was triggered by the EffectManager. 
     *  This base class version saves the event that triggered the effect
     *  in the <code>triggerEvent</code> property.
     *  Each subclass should override this method.
     * 
     *  @param event The Event object that was dispatched
     *  to trigger the effect.
     *  For example, if the trigger was a mouseDownEffect, the event
     *  would be a MouseEvent with type equal to MouseEvent.MOUSEDOWN. 
     }
    procedure Init(aTrigger: string);
    {
     *  Plays the animation instance on the target after the
     *  <code>startDelay</code> period has elapsed.
     *  Called by the TGRAnimator class.
     *  Use this function instead of the <code>play()</code> method
     *  when starting an TGRAnimatorInstance.
     }
    procedure Start;

    
    {*
     *  Called by the <code>GotoEnd()</code> method when the animation
     *  finishes playing.
     *  This function dispatches an <code>endEffect</code> event(message)
     *  for the target.
     *
     *  <p>You do not have to override this method in a subclass.
     *  You do not need to call this method when using effects,
     *  but you may need to call it if you create an effect subclass.</p>
     *
     *  @see mx.events.EffectEvent
     *}
    procedure Finish();
    
    {*
     *  Called after each iteration of a repeated effect finishes playing.
     *
     *  <p>You do not have to override this method in a subclass.
     *  You do not need to call this method when using animators.</p>
     *}
    function FinishRepeat();
  public
    {
        The TGRAnimator object that created this AnimatorInstance object.
     }
    property Owner: TGRAnimator;
  end;

  TGRDynTargets = array of TObject;
  //Manage the AnimatorInstance.
  {
  The TGRAnimator class is an abstract base class that defines the basic 
  functionality of all animation effects.
  The TGRAnimator class defines the base factory class for all TAnimatorInstance subclasses.
  }
  TGRAnimator = class(TGRCustomAnimator)
  protected
    FTargets: TObjectList;
  public
    {*
     *  Takes an Array of target objects and invokes the 
     *  <code>createInstance()</code> method on each target. 
     *
     *  @param aTargets the Array of objects to animate with this animator.
     *
     *}
    procedure CreateInstances(const aTargets: TGRDynTargets);
    {
     *  Creates a single effect instance and initializes it.
     *  Use this method instead of the <code>play()</code> method
     *  to manipulate the effect instance properties
     *  before the effect instance plays. 
     *  
     *  <p>The effect instance is created with the type 
     *  specified in the <code>instanceClass</code> property.
     *  It is then initialized using the <code>initInstance()</code> method. 
     *  If the instance was created by the EffectManager 
     *  (when the effect is triggered by an effect trigger), 
     *  the effect is further initialized by a call to the 
     *  <code>EffectInstance.initEffect()</code> method.</p>
     * 
     *  <p>Calling the <code>createInstance()</code> method 
     *  does not play the effect.
     *  You must call the <code>startEffect()</code> method
     *  on the returned effect instance. </p>
     *
     *  <p>This function is automatically called by the 
     *  <code>Animator.play()</code> method. </p>
     *
     *  @param target Object to animate with this animator.
     *
     *  @return The animator instance object for the animator.
     }
    function CreateInstance(const aTarget: TObject = nil): TGRAnimatorInstance;
    {
     *  Removes event listeners from an instance
     *  and removes it from the list of instances.
     }
    function DeleteInstance(const aInstance: TGRAnimatorInstance);

    {*
     *  Begins playing the effect.
     *  You typically call the <code>end()</code> method 
     *  before you call the <code>play()</code> method
     *  to ensure that any previous instance of the effect
     *  has ended before you start a new one.
     *
     *  <p>All subclasses must implement this method.</p>
     *
     *  @param aTargets Array of target objects on which to play this effect.
     *  If this parameter is specified, then the old animator's <code>targets</code>
     *  property will be set to this..
     *
     *  @param playReversedFromEnd If <code>true</code>,
     *  play the effect backwards.
     *
     *} 
    procedure Play(const aPlayReversedFromEnd:Boolean = false): override; overload;
    procedure Play(const aTargets: TGRDynTargets; const aPlayReversedFromEnd:Boolean = false); overload;

    property IsPlaying: Boolean;
    //return the first object in the Targets if any.
    property Target;
    property Targets: TObjectList;
    property OnAniStart: TNotifyEvent;
    property OnAniEnd: TNotifyEvent;
  end;
  
implementation

uses
  Consts;

const
  G32DefaultDelay: ShortInt = 100; // Time in ms.
  G32MinimumDelay: ShortInt = 10;  // Time in ms.


end.
