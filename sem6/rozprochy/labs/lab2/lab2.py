from fastapi import FastAPI
from enum import Enum
from fastapi.responses import JSONResponse

app=FastAPI( )

# Lab
class Vote:
    def __init__(self, id, poll_id, field_name):
        self.id = id
        self.poll_id = poll_id
        self.field_name = field_name

    def edit_vote(self, field_name):
        self.field_name = field_name

    def return_dict(self):
        return {"vote_id": self.id, "poll_id": self.poll_id, "field": self.field_name}

class Poll:
    def __init__(self, id, name, fields):
        self.id = id
        self.name = name
        self.fields = {}
        for field in fields:
            self.fields[field] = set()

    def edit_poll(self, name=None, fields=None):
        if name is not None:
            self.name = name
        if fields is not None:
            self.fields = {}
            for field in fields:
                self.fields[field] = set()

    def vote(self, vote_id, field):
        self.fields[field].add(vote_id)

    def edit_vote(self, vote_id, old_field, new_field):
        if vote_id in self.fields[old_field]:
            self.fields[old_field].remove(vote_id)
            self.fields[new_field].add(vote_id)

    def delete_vote(self, vote_id, field):
        self.fields[field].remove(vote_id)

    def return_dict(self):
        return {"poll_id": self.id, "name": self.name, "fields": list(self.fields.keys())}

    def return_results(self):
        results = {}
        for field in self.fields.keys():
            results[field] = len(self.fields[field])
        return {"poll_id": self.id, "name": self.name, "results": results}


polls = {}
poll_id = 0
votes = {}
vote_id = 0

@app.post("/polls/create_poll")
async def create_poll(name: str, fields: list[str]):
    global poll_id
    poll_id += 1
    poll = Poll(poll_id, name, fields)
    polls[poll_id] = poll
    return JSONResponse(status_code=status.HTTP_201_CREATED, content=poll.return_dict())

@app.put("/polls/edit_poll/{poll_id}")
async def edit_poll(poll_id: int, name: str, fields: list[str]):
    if poll_id not in polls.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "poll not found"})
    polls[poll_id].edit_poll(name, fields)
    return polls[poll_id].return_dict()

@app.delete("/polls/delete_poll/{poll_id}")
async def delete_poll(poll_id: int):
    if poll_id not in polls.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "poll not found"})
    poll = polls[poll_id]
    for field in poll.fields.keys():
        for vote_id in poll.fields[field]:
            del votes[vote_id]
    del polls[poll_id]
    return {"message": "ok"}

@app.post("/polls/vote")
async def vote(poll_id: int, field_name: str):
    if poll_id not in polls.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "poll not found"})
    poll = polls[poll_id]
    if field_name not in poll.fields.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "field not found"})
    
    global vote_id
    vote_id += 1
    vote = Vote(vote_id, poll_id, field_name)
    votes[vote_id] = vote
    poll.vote(vote_id, field_name)
    return JSONResponse(status_code=status.HTTP_201_CREATED, content=vote.return_dict())

@app.put("/polls/edit_vote/{vote_id}")
async def edit_vote(vote_id: int, new_field: str):
    if vote_id not in votes.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "vote not found"})
    vote = votes[vote_id]
    poll = polls[vote.poll_id]
    if new_field not in poll.fields.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "field not found"})
    poll.edit_vote(vote_id, vote.field_name, new_field)
    vote.edit_vote(new_field)
    return vote.return_dict()

@app.delete("/polls/delete_vote/{vote_id}")
async def delete_vote(vote_id: int):
    if vote_id not in votes.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "vote not found"})
    vote = votes[vote_id]
    poll = polls[vote.poll_id]
    poll.delete_vote(vote_id, vote.field_name)
    del votes[vote_id]
    return {"message": "ok"}

@app.get("/polls/view_all_polls")
async def view_all_polls():
    return [poll.return_results() for poll in polls.values()]

@app.get("/polls/view_poll/{poll_id}")
async def view_poll(poll_id: int):
    if poll_id not in polls.keys():
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content={"message": "poll not found"})
    poll = polls[poll_id]
    return poll.return_results()

