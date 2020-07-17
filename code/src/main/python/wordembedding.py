import transformers
import torch
import numpy as np
import json
import sys


# https://www.aclweb.org/anthology/S17-2001/
# https://arxiv.org/pdf/1810.04805.pdf

def get_bert_pretrained():
    with open("config.json", "r") as json_file:
        text = json.loads(json_file.read())
    return text["bert"]

bert_pretrained = get_bert_pretrained()

tokenizer = transformers.BertTokenizer.from_pretrained(bert_pretrained)
bert = transformers.BertModel.from_pretrained(bert_pretrained)


def get_encoding(sentence: str):
    encoded_text = torch.tensor(tokenizer.encode(sentence)).unsqueeze(0)
    return bert(encoded_text)[0].squeeze(0).cpu().detach().numpy()


def cos_sim(v1, v2):
    return np.dot(v1, v2) / (np.linalg.norm(v1) * np.linalg.norm(v2))


def similarity(w1: str, w2: str):
    v1 = get_encoding(w1)
    v1 = v1[0, :].reshape((1, v1.shape[1]))
    v2 = get_encoding(w2)
    v2 = v2[0, :].reshape((1, v2.shape[1]))
    return np.max([cos_sim(v1[i, :], v2[j, :])
               for i in range(v1.shape[0])
               for j in range(v2.shape[0])])


if __name__ == "__main__":
    print(similarity(sys.argv[1], sys.argv[2]))
