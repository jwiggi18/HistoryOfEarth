// code for generateQuiz from: https://simplestepscode.com/javascript-quiz-tutorial/
function generateQuiz(questions, quizContainer, resultsContainer, submitButton){

    var quizContainer = document.getElementById('quiz');
    var resultsContainer = document.getElementById('results');
    var submitButton = document.getElementById('submit');

    function showQuestions(questions, quizContainer){
    	// we'll need a place to store the output and the answer choices
    	var output = [];
    	var answers;

    	// for each question...
    	for(var i=0; i<questions.length; i++){

    		// first reset the list of answers
    		answers = [];

    		// for each available answer to this question...
    		for(letter in questions[i].answers){

    			// ...add an html radio button
    			answers.push(
    				'<label>'
    					+ '<input type="radio" name="question'+i+'" value="'+letter+'">'
    					+ letter + ': '
    					+ questions[i].answers[letter]
    				+ '</label>'
    			);
    		}

    		// add this question and its answers to the output
    		output.push(
    			'<div class="question">' + questions[i].question + '</div>'
    			+ '<div class="answers">' + answers.join('') + '</div>'
    		);
    	}

    	// finally combine our output list into one string of html and put it on the page
    	quizContainer.innerHTML = output.join('');
    }
    // end showQuestions function

    showQuestions(questions, quizContainer);

    function showResults(questions, quizContainer, resultsContainer){

    	// gather answer containers from our quiz
    	var answerContainers = quizContainer.querySelectorAll('.answers');

    	// keep track of user's answers
    	var userAnswer = '';
    	var numCorrect = 0;

    	// for each question...
    	for(var i=0; i<questions.length; i++){

    		// find selected answer
    		userAnswer = (answerContainers[i].querySelector('input[name=question'+i+']:checked')||{}).value;

    		// if answer is correct
    		if(userAnswer===questions[i].correctAnswer){
    			// add to the number of correct answers
    			numCorrect++;

    			// color the answers green
    			answerContainers[i].style.color = 'lightgreen';
    		}
    		// if answer is wrong or blank
    		else{
    			// color the answers red
    			answerContainers[i].style.color = 'red';
    		}
    	}

    	// show number of correct answers out of total
    	resultsContainer.innerHTML = numCorrect + ' out of ' + questions.length;
    }
    // end showResults function

    // show the questions
    showQuestions(questions, quizContainer);

    //on submit, show results
    submitButton.onclick = function() {
      showResults(questions, quizContainer, resultsContainer);
    }


}

function taxonQuiz () {
  var group = document.getElementById("group").value;
  var overview = [
      {
        question:"The “tail of life on earth has been unfolding for about ________” years (0.08) ",
        answers: {
                  a: '4 thousand',
                  b: '4 billion'
              },
              correctAnswer: 'b'
      },
      {
        question: "The system that we use to “bind” the chapters of earth’s history together is called the _____________ (0.48)",
        answers: {
                 a: 'Geologic Time Scale',
                 b: 'Strata'
               },
               correctAnswer: 'a'
      },
      {
        question: 'The layers of rock composing earth’s surface are called the _______ (1.10)',
        answers: {
                a: 'Geologic Time Scale',
                b: 'Strata'
                },
                correctAnswer: 'b'
      },
      {
        question: 'Rock layers don’t appear in the same order all over the world so there was no way for geologists to compare rocks from one location to another. The solution to the problem of not being able to compare strata between locations was ________ (1.48)',
        answers: {
                a: 'Carbon Dioxide',
                b: 'Fossils'
                },
                correctAnswer: 'b'
      },
      {
        question: 'If you were to find a trilobite fossil you would know you were looking at rock that is older than when _________ lived. (2.19)',
        answers: {
                a: 'Ammonites',
                b: 'Trilobites'
                },
                correctAnswer: 'a'
      },
      {
        question: 'Name the five subgroups of geologic time from longest to shortest expanse of time (2:48)',
        answers: {
                a: 'Eons, Eras, Periods, Epochs,  Ages',
                b: 'Periods, Eons, Ages, Eras, Epochs'
                },
                correctAnswer: 'a'
      },
      {
        question: 'Eons can be how long? (3.33)',
        answers: {
                a: 'One million years long',
                b: 'A half billion to nearly 2 billion years long'
                },
                correctAnswer: 'b'
      },
      {
        question: 'The earliest eon, the Hadeon (4.6 - 4 bya), is named after: (3:48)',
        answers: {
                a: 'Hadrians Wall',
                b: 'The Greek Underworld'
                },
                correctAnswer: 'b'
      },
      {
        question: 'The atmosphere of the Archean (4 - 2.5 bya) eon was mostly: (4.45)',
        answers: {
                a: 'Carbon Dioxide',
                b: 'Nitrogen'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The __________ eon saw the rise of eukaryotes which are characterized by cells with a nucleus and organelles contained in membranes and extended from 2.5 billion to 541 million years ago. (5.11)',
        answers: {
                a: 'Panerazoic',
                b: 'Proterozoic'
                },
                correctAnswer: 'b'
      },
      {
        question: 'Some ___________ developed into the first complex forms of life before the current eon ever began. (5.20)',
        answers: {
                a: 'eukaryotes',
                b: 'prokaryotes'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The current eon is the ________ and its name means “visible life” and it extends from 541 mya to the present. (5:28)',
        answers: {
                a: 'Panerazoic',
                b: 'Proterozoic'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The first era of our current eon, the Paleozoic, was defined by the:  (6.02)',
        answers: {
                a: 'diversification of visible life',
                b: 'K-PG extinction'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The first widespread organisms to evolve “hard parts” that could be fossilized were: (6:35)',
        answers: {
                a: 'Trilobites',
                b: 'Graptolites'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The supercontinent that existed 299 million years ago was called:(7:10)',
        answers: {
                a: 'North America',
                b: 'Pangea'
                },
                correctAnswer: 'b'
      },
      {
        question: 'The most severe extinction in our planets history, known as The Great Dying, occured at the end of the Paleozoic era and wiped out 70% of land vertebrates and _____% of marine species. (7:29)',
        answers: {
                a: '55',
                b: '96'
                },
                correctAnswer: 'b'
      },
      {
        question: 'Which Era is known as “The age of reptiles”?',
        answers: {
                a: 'Mesozoic',
                b: 'Cretaceous'
                },
                correctAnswer: 'a'
      },
      {
        question: 'All of the non-avian _________ lived only in the Mesozoic Era. (8.24)',
        answers: {
                a: 'Dinosaurs',
                b: 'Mammals'
                },
                correctAnswer: 'a'
      },
      {
        question: 'What evidence of an asteroid impact at the end of the Mesozoic do scientists find in the strata? (9:03)',
        answers: {
                a: 'A layer of Iridium',
                b: 'Alien life'
                },
                correctAnswer: 'a'
      },
      {
        question: 'What is “our Era”?',
        answers: {
                a: 'The Cenezoic',
                b: 'The Paleozoic'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The Cenozoic Era is characterized by the rise of the (9.55)',
        answers: {
                a: 'Mammals',
                b: 'Dinosaurs'
                },
                correctAnswer: 'a'
      },
      {
        question: 'The growth of the polar ice caps contributed to the development of _________, a new habitat in which early horses and big cats evolved. ',
        answers: {
                a: 'Tropical Forests',
                b: 'Grasslands'
                },
                correctAnswer: 'b'
      },
      {
        question: '15 thousand years ago the climate began to ________ leading to the extinction of many of the giant fauna (really big animals). 10.50',
        answers: {
                a: 'warm',
                b: 'cool'
                },
                correctAnswer: 'a'
      }

  ];
  var paleozoic = [
      {
        question:"questions coming soon",
        answers: {
                  a: '',
                  b: ''
              },
              correctAnswer: 'a'
      },
      {
        question: "",
        answers: {
                 a: '',
                 b: ''
               },
               correctAnswer: 'a'
      },
      {
        question: '',
        answers: {
                  a: '',
                  b: ''
                },
                correctAnswer: 'b'
      }
  ];
  var mesozoic = [
      {
        question:"questions coming soon",
        answers: {
                  a: '',
                  b: ''
              },
              correctAnswer: 'a'
      },
      {
        question: "",
        answers: {
                 a: '',
                 b: ''
               },
               correctAnswer: 'a'
      },
      {
        question: '',
        answers: {
                  a: '',
                  b: ''
                },
                correctAnswer: 'b'
      }
  ];
  var cenozoic = [
      {
        question:"questions coming soon",
        answers: {
                  a: '',
                  b: ''
              },
              correctAnswer: 'b'
      },
      {
        question: "",
        answers: {
                 a: '',
                 b: ''
               },
               correctAnswer: 'a'
      },
      {
        question: '',
        answers: {
                  a: '',
                  b: ''
                },
                correctAnswer: 'b'
      }
  ];

  if (group == "overview") {generateQuiz(questions=overview)}
  else if (group =="paleozoic"){generateQuiz(questions=paleozoic)}
  else if (group =="mesozoic"){generateQuiz(questions=mesozoic)}
  else if (group =="cenozoic"){generateQuiz(questions=cenozoic)}
  else {document.getElementById("error").innerHTML = "oops, something went wrong";}
}
