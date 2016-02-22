// ############################## Helper functions ##############################

// Shows slides. We're using jQuery here - the **$** is the jQuery selector function, which takes as input either a DOM element or a CSS selector string.
function showSlide(id) {
  // Hide all slides
  $(".slide").hide();
  // Show just the slide we want to show
  $("#"+id).show();
}

// Get random integers.
// When called with no arguments, it returns either 0 or 1. When called with one argument, *a*, it returns a number in {*0, 1, ..., a-1*}. When called with two arguments, *a* and *b*, returns a random value in {*a*, *a + 1*, ... , *b*}.
function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}

// Add a random selection function to all arrays (e.g., <code>[4,8,7].random()</code> could return 4, 8, or 7). This is useful for condition randomization.
Array.prototype.random = function() {
  return this[random(this.length)];
}

// shuffle function - from stackoverflow?
// shuffle ordering of argument array -- are we missing a parenthesis?
function shuffle (a)
{
    var o = [];

    for (var i=0; i < a.length; i++) {
      o[i] = a[i];
    }

    for (var j, x, i = o.length;
         i;
         j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

// from: http://www.sitepoint.com/url-parameters-jquery/
$.urlParam = function(name){
  var results = new RegExp('[\?&]' + name + '=([^&#]*)').exec(window.location.href);
  if (results==null){
    return null;
  } else{
    return results[1] || 0;
  }
}

// ############################## Configuration settings ##############################
var faces = [];
var NUM_TRIALS_PER_DFT = 3;
for (var i = 0; i < NUM_TRIALS_PER_DFT; i++) {
  for (var dft = 0; dft <= 100; dft += 10) {
    faces.push(dft);
  }
}

function getFaceFile(dft) {
  if (dft == 0) dft = '00';
  return 'images/EXP1_DFT_' + dft + '.jpg';
}

faces = shuffle(faces);

var totalTrials = faces.length;

// Initialize trial to trustworthy or attractive
var type = Math.random();
if (type >= 0.5) {
  type = 'attractive';
  $('.attractiveness-instr').show();
  $('.trustworthy-instr').hide();
} else {
  type = 'trustworthy';
  $('.attractiveness-instr').hide();
  $('.trustworthy-instr').show();
}

// Show the instructions slide -- this is what we want subjects to see first.
showSlide("instructions");

// ############################## The main event ##############################
var experiment = {

    // The object to be submitted.
    data: {
      type: [],
      age: [],
      gender: [],
      education: [],
      race: [],
      face: [],
      face_file: [],
      rating: [],
      elapsed_ms: [],
      num_errors: [],
      expt_aim: [],
      expt_gen: [],
      user_agent: [],
      window_width: [],
      window_height: [],
    },

    start_ms: 0,  // time current trial started ms
    num_errors: 0,    // number of errors so far in current trial

    // end the experiment
    end: function() {
      showSlide("finished");
      setTimeout(function() {
        turk.submit(experiment.data)
      }, 1500);
    },

    // LOG RESPONSE
    log_response: function() {
      var response_logged = false;
      var elapsed = Date.now() - experiment.start_ms;

      //Array of radio buttons
      var radio = document.getElementsByName("judgment");

      // Loop through radio buttons
      for (i = 0; i < radio.length; i++) {
        if (radio[i].checked) {
          experiment.data.rating.push(radio[i].value);
          experiment.data.elapsed_ms.push(elapsed);
          experiment.data.num_errors.push(experiment.num_errors);
          response_logged = true;
        }
      }

      if (response_logged) {
        nextButton.blur();

        // uncheck radio buttons
        for (i = 0; i < radio.length; i++) {
          radio[i].checked = false
        }

        $('#stage-content').hide();
        experiment.next();
      } else {
          experiment.num_errors += 1;
          $("#testMessage").html('<font color="red">' +
               'Please make a response!' +
               '</font>');
      }
    },

    // The work horse of the sequence - what to do on every trial.
    next: function() {
      // Allow experiment to start if it's a turk worker OR if it's a test run
      if (window.self == window.top | turk.workerId.length > 0) {
          $("#testMessage").html('');   // clear the test message
          $("#prog").attr("style","width:" +
              String(100 * (1 - faces.length/totalTrials)) + "%")
          // style="width:progressTotal%"
          window.setTimeout(function() {
            $('#stage-content').show();
            experiment.start_ms = Date.now();
            experiment.num_errors = 0;
          }, 150);

          // Get the current trial - <code>shift()</code> removes the first element
          // select from our scales array and stop exp after we've exhausted all the domains
          var face_dft = faces.shift();

          //If the current trial is undefined, call the end function.
          if (typeof face_dft == "undefined") {
            return experiment.debriefing();
          }
          var face_filename = getFaceFile(face_dft);
          experiment.data.face_file.push(face_filename);

          // Display the sentence stimuli
          $("#face").attr('src', face_filename);


          // push all relevant variables into data object
          experiment.data.face.push(face_dft);
          experiment.data.window_width.push($(window).width());
          experiment.data.window_height.push($(window).height());

          showSlide("stage");
      }
    },

    //  go to debriefing slide
    debriefing: function() {
      showSlide("debriefing");
    },

    // submitcomments function
    submit_comments: function() {
      var races = document.getElementsByName("race[]");
      for (i = 0; i < races.length; i++) {
        if (races[i].checked) {
          experiment.data.race.push(races[i].value);
        }
      }
      experiment.data.age.push(document.getElementById("age").value);
      experiment.data.gender.push(document.getElementById("gender").value);
      experiment.data.education.push(document.getElementById("education").value);
      experiment.data.expt_aim.push(document.getElementById("expthoughts").value);
      experiment.data.expt_gen.push(document.getElementById("expcomments").value);
      experiment.data.type.push(type);
      experiment.data.user_agent.push(window.navigator.userAgent);
      experiment.end();
    }
}

$(function() {
  $('form#demographics').validate({
    rules: {
      "age": "required",
      "gender": "required",
      "education": "required",
      "race[]": "required",
    },
    messages: {
      "age": "Please choose an option",
      "gender": "Please choose an option",
      "education": "Please choose an option",
    },
    submitHandler: experiment.submit_comments
  });
  $('#race_group input[value=no_answer]').click(function() {
    $('#race_group input').not('input[value=no_answer]').attr('checked', false);
  });
  $('#race_group input').not('input[value=no_answer]').click(function() {
    $('#race_group input[value=no_answer]').attr('checked', false);
  });
});

