
{$DEFINE FASTPLAZ}

{$DEFINE short_url}
{disabled $DEFINE GREYHOUND}
{disabled $DEFINE DEBUG}
{disabled $DEFINE SYNAPSE}

{$ifdef SYNAPSE}
  {disabled $DEFINE XMAILER}
{$endif}

{ $DEFINE dOPF}
{ $DEFINE zeos}

// requirement : SynEdit package
{ $DEFINE syntext}

{disable $DEFINE HEAPTRACE}


// INTEGRATION
{$DEFINE SLACK_INTEGRATION}
{$DEFINE NEWRELIC_INTEGRATION}

