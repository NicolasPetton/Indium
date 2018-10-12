# Indium server

This documentation describes the version 3.3 of the server component of
[Indium](https://indium.readthedocs.org).

The Indium server exposes a simple interface to communicate with a JS runtime
for debugging.  It currently supports Chrome/Chromium (debug protocol 1.2), and
NodeJS >= 8.

The server works as an adapter in the communication with a JavaScript runtime,
and transparently handles concerns like file to script location conversion,
sourcemap resolution and management, breakpoint resolution, etc., providing a
streamlined interface where the client doesn't have to deal with script sources
and URLs, but files on disk.

## Installation

    npm install -g indium

## Stating the server

Run the following command:

    indium PORT
	
The default port used by the Emacs client is `13840`.

## Protocol

Communication is done through a TCP connection using the JSON format.
Each message, from the server or the client, is terminated with a line feed.

### 1. Overview

The client communicates with the server through JSON messages, referred to as
[requests](#requests).  The server answers with corresponding JSON messages,
referred to as [responses](#responses).

The response order is not guaranteed to follow the request order.  In order to
identify what request a specific response is emitted for, each request must
contain a unique `"id"` field.  Its response will also contain an `"id"` field
with the same value.

For each request, the client is guaranteed to receive at some point in time a
response.  The response can either be successful, or unsuccessful.

Responses contain a `"type"` field to identify whether the request completed
successfully or not.  Successful responses have their `"type"` feild set to
`"success"`, unsuccessful responses to `"error"`.

The server can initiate communication with the client through
[notification](#notifications) JSON messages.  Notification messages have no
`"id"`, as they are not sent in response to a request.


#### <a name="requests"></a>Requests

Requests must follow the following general structure:

| Key     | Type                    | Description                                                                                                                                         |
|:--------|:------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------|
| id      | `<string>`              | Id of the request                                                                                                                                   |
| type    | `<string>`              | Type of request.  The type specifies the category of the request.  Valid types are `"version"`, `"connection"`, `"configurations"` and `"runtime"`. |
| payload | `<object>` *(optional)* | Payload of the request.  Contains the details specific to the request.                                                                              |

`payload` objects must specify an `action` key.  The `action` key determines the
action to be executed by the server.  Valid actions depend the on `type` of
request.

#### <a name="responses"></a>Responses

Each response contains the `id` of the corresponding request and follows the
following structure:

| Key     | Type                    | Description                                       |
|:--------|:------------------------|:--------------------------------------------------|
| id      | `<string>`              | Id of the request the response is for             |
| type    | `<string>`              | Type of response, either `"success"` or `"error"` |
| payload | `<object>` *(optional)* | Optional payload of the response                  |

### 2. `configurations` requests

The server is responsible for looking up the `.indium.json` configuration file
from a project directory.

#### list

The `list` action fetches the available configurations for a directory.

*Request payload:*

| Key       | Type or value | Description                                                         |
|:----------|:--------------|:--------------------------------------------------------------------|
| action    | `"list"`      | Action type                                                         |
| directory | `<string>`    | Directory where the `.indium.json` project file look up is started. |

*Successful response payload:*

Array of configurations.

### 3. `connection` request type

`connection` requests are used to open/close connections to a runtime.

#### connect

*Request payload:*

| Key       | Type or value | Description                                                         |
|:----------|:--------------|:--------------------------------------------------------------------|
| action    | `"connect"`   | Action type                                                         |
| directory | `<string>`    | Directory where the `.indium.json` project file look up is started. |
| name      | `<string>`    | Name of the configuration to choose for the runtime connection.     |

*Successful response payload:*

No payload.

#### close

*Request payload:*

| Key    | Type or value | Description |
|:-------|:--------------|:------------|
| action | `"close"`     | Action type |

*Successful response payload:*

No payload, but the server exits.


### 4. `runtime` request type

The `runtime` request type is the most important type of request as evaluation,
inspection and debugging is done through runtime requests.

#### evaluate

Evaluate an expression and send the result back.  If `frameId` is in
the payload, evaluation occurs within the context of the associated
call frame. If `frameId` is not provided but there is an active
debugging session, the expression is evaluated within the context of
the top frame.

*Request payload:*

| Key        | Type or value           | Description            |
|:-----------|:------------------------|:-----------------------|
| action     | `"evaluate"`            | Action type            |
| expression | `<string>`              | Expression to evaluate |
| frameId    | `<string>` *(optional)* | Call frame id          |

*Successful response payload:*

A [remote object](#remote-object).

#### getCompletion

The `getCompletion` request provides auto-completion on expression strings.

*Request payload:*

| Key        | Type or value     | Description            |
|:-----------|:------------------|:-----------------------|
| action     | `"getCompletion"` | Action type            |
| expression | `<string>`        | Expression to complete |

*Successful response payload:*

An array of completion candidate strings.

#### activateBreakpoints

*Request payload:*

| Key    | Type or value           | Description |
|:-------|:------------------------|:------------|
| action | `"activateBreakpoints"` | Action type |

*Successful response payload:*

No payload.

#### deactivateBreakpoints

*Request payload:*


| Key    | Type or value             | Description |
|:-------|:--------------------------|:------------|
| action | `"deactivateBreakpoints"` | Action type |


*Successful response payload:*

No payload.

#### addBreakpoint

Breakpoint registration works asynchronously.  The client requests the addition
of a breakpoint at a location using `addBreakpoint` requests.  

Breakpoint resolution might use sourcemaps internally, without exposing the
details to the client.

Once the breakpoint has been resolved (e.g. the breakpoint was successfully
registered in the runtime), the server sends a [`breakpointResolved`
notification](#breakpointResolved) with the resolved location.

*Request payload:*

| Key    | Type or value     | Description                                   |
|:-------|:------------------|:----------------------------------------------|
| action | `"addBreakpoint"` | Action type                                   |
| id     | `<string>`        | Unique ID of the breakpoint to be added       |
| file   | `<file>`          | File where the breakpoint is added            |
| line   | `<line>`          | Line of the file where the breakoint is added |

*Successful response payload:*

No payload.

#### removeBreakpoint

*Request payload:*

| Key    | Type or value        | Description                                   |
|:-------|:---------------------|:----------------------------------------------|
| action | `"removeBreakpoint"` | Action type                                   |
| id     | `<string>`           | Unique ID of the breakpoint to be removed     |

*Successful response payload:*

No payload.

#### resume

*Request payload:*

| Key    | Type or value | Description |
|:-------|:--------------|:------------|
| action | `"resume"`    | Action type |

*Successful response payload:*

No payload.

#### stepInto

*Request payload:*

| Key    | Type or value | Description |
|:-------|:--------------|:------------|
| action | `"stepInto"`  | Action type |

*Successful response payload:*

No payload.

#### stepOut

*Request payload:*

| Key    | Type or value | Description |
|:-------|:--------------|:------------|
| action | `"stepOut"`   | Action type |

*Successful response payload:*

No payload.

#### stepOver

*Request payload:*

| Key    | Type or value | Description |
|:-------|:--------------|:------------|
| action | `"stepOver"`  | Action type |

*Successful response payload:*

No payload.

#### continueToLocation

*Request payload:*

| Key      | Type or value          | Description                                                     |
|:---------|:-----------------------|:----------------------------------------------------------------|
| action   | `"continueToLocation"` | Action type                                                     |
| location | `<location>`           | [Location](#location) to jump to.                               |
| name     | `<string>`             | Name of the configuration to choose for the runtime connection. |

*Successful response payload:*

No payload.

#### getSource

*Request payload:*

| Key    | Type or value | Description            |
|:-------|:--------------|:-----------------------|
| action | `"getSource"` | Action type            |
| id     | `<string>`    | ID of a runtime script |

*Successful response payload:*

A string of the contents of the requested script.

### 5. <a name="notifications"></a>server notifications

#### <a name="breakpointResolved"></a>breakpointResolved notifications

*Request payload:*
*Successful response payload:*

#### paused notifications

This notification is triggered when the debugger is paused at a
specific call frame.

*Notification payload:*

| Key         | Type or value   | Description                                              |
|:------------|:----------------|:---------------------------------------------------------|
| frames      | `<callFrame>[]` | Array of call frames (`id`, `functionName`, `location`…) |
| reason      | `<string>`      | Why the debugger stopped                                 |
| description | `<string>`      | Additional information (e.g., stack trace)               |

#### resumed notifications

*Request payload:*
*Successful response payload:*

### 6. Objects in payloads

#### <a name="remote-object"></a>Remote object

A remote object represents an object in the runtime.
It can either contain:

- a value, with a description (for objects like scalars);
- a reference to an object (with an id), for objects, arrays, functions, etc.

| Key         | Type                    | Description                                                             |
|:------------|:------------------------|:------------------------------------------------------------------------|
| description | `<string>`              | Short description string of the object                                  |
| id          | `<string>` *(optional)* | ID of the referenced object                                             |
| type        | `<string>` *(optional)* | Type of the remote object.  Only valid if the `id` feild is set         |
| preview     | `<string>` *(optional)* | Longer description of the remote object, to be used as a display string |

#### <a name="location"></a>Location

A location represents a position in a file.

| Key    | Type                    | Description    |
|:-------|:------------------------|:---------------|
| file   | `<string>`              | File on disk   |
| line   | `<number>`              | 1-based line   |
| column | `<number>` *(optional)* | 0-based column |

### 7. Examples

In the the following example, `>>` represents inputs and `<<` represents outputs.

	>> {"id": "1", "type": "configurations", "payload": {"action": "list", "directory": "/home/user/proj/"}}
	<< {"id": "1", "type": "success", "payload": [{"type": "chrome", "name": "Web Project", "root": "./src"}]}
    >> {"id": "2", "type": "connection", "payload": {"action": "connect", "name": "Web Project", "directory": "/home/user/proj/""}}	
	<< {"id": "2", "type": "success"}
	>> {"id": "3", "type": "runtime", "payload": {"action": "evaluate", "expression": "1+2"}}
	<< {"id": "3", "type": "success", "payload": {"description": "3"}}
    >> {"id": "4", "type": "connection", "payload": {"action": "close"}}
	(TCP connection closed)
