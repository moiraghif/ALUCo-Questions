# import sys
import json

import cherrypy
import transformers
import torch

import numpy as np
from pandas import DataFrame


def cos_sim(v1, v2):
    return np.dot(v1, v2) / (np.linalg.norm(v1) * np.linalg.norm(v2))


class BertServer:
    
    @cherrypy.expose
    @cherrypy.tools.json_out()
    @cherrypy.tools.json_in()
    def process(self):
        data = cherrypy.request.json
        # print(json_file)
        # data = DataFrame(json_file)
        # data = json.load(json_file)
        sim = self.similarity(data["sent1"], data["sent2"])
        return json.dumps({"cosine": str(sim)})

    def get_encoding(self, sentence: str):
        encoded_text = torch.tensor(self.tokenizer.encode(sentence)).unsqueeze(0)
        return self.bert(encoded_text)[0].squeeze(0).cpu().detach().numpy()

    def similarity(self, w1: str, w2: str):
        v1 = self.get_encoding(w1)
        v1 = v1[0, :].reshape((1, v1.shape[1]))
        v2 = self.get_encoding(w2)
        v2 = v2[0, :].reshape((1, v2.shape[1]))
        return np.max([cos_sim(v1[i, :], v2[j, :])
                  for i in range(v1.shape[0])
                  for j in range(v2.shape[0])])

    def get_bert_pretrained(self):
        with open("config.json", "r") as config_file:
            text = json.loads(config_file.read())
        return text["bert"]

    def __init__(self):
        # https://www.aclweb.org/anthology/S17-2001/
        # https://arxiv.org/pdf/1810.04805.pdf
        bert_version = self.get_bert_pretrained() 
        self.tokenizer = transformers.BertTokenizer.from_pretrained(bert_version)
        self.bert = transformers.BertModel.from_pretrained(bert_version)


if __name__ == "__main__":
    config = {'server.socket_host': '0.0.0.0'}
    cherrypy.config.update(config)
    cherrypy.quickstart(BertServer())
